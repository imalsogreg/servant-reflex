{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Servant.Common.Req where

-------------------------------------------------------------------------------
import           Control.Applicative        (liftA2, liftA3)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Reflex.Dom
import           Servant.Common.BaseUrl     (BaseUrl, showBaseUrl, SupportsServantReflex)
import           Servant.API.ContentTypes   (MimeUnrender(..), NoContent(..))
import           Web.HttpApiData            (ToHttpApiData(..))
-------------------------------------------------------------------------------
import           Servant.API.BasicAuth



data ReqResult a = ResponseSuccess a XhrResponse
                 | ResponseFailure Text XhrResponse
                 | RequestFailure Text

instance Functor ReqResult where
  fmap f (ResponseSuccess a xhr) = ResponseSuccess (f a) xhr
  fmap _ (ResponseFailure r x)   = ResponseFailure r x
  fmap _ (RequestFailure r)      = RequestFailure r

reqSuccess :: ReqResult a -> Maybe a
reqSuccess (ResponseSuccess x _) = Just x
reqSuccess _                     = Nothing

reqFailure :: ReqResult a -> Maybe Text
reqFailure (ResponseFailure s _) = Just s
reqFailure (RequestFailure s)    = Just s
reqFailure _                     = Nothing

response :: ReqResult a -> Maybe XhrResponse
response (ResponseSuccess _ x) = Just x
response (ResponseFailure _ x) = Just x
response _                     = Nothing


-------------------------------------------------------------------------------
-- | You must wrap the parameter of a QueryParam endpoint with 'QParam' to
-- indicate whether the parameter is valid and present, validly absent, or
-- invalid
data QParam a = QParamSome a
              -- ^ A valid query parameter
              | QNone
              -- ^ Indication that the parameter is intentionally absent (the request is valid)
              | QParamInvalid Text
              -- ^ Indication that your validation failed (the request isn't valid)

qParamToQueryPart :: ToHttpApiData a => QParam a -> Either Text (Maybe Text)
qParamToQueryPart (QParamSome a)    = Right (Just $ toQueryParam a)
qParamToQueryPart QNone             = Right Nothing
qParamToQueryPart (QParamInvalid e) = Left e

data QueryPart t = QueryPartParam  (Dynamic t (Either Text (Maybe Text)))
                 | QueryPartParams (Dynamic t [Text])
                 | QueryPartFlag   (Dynamic t Bool)


-------------------------------------------------------------------------------
-- The data structure used to build up request information while traversing
-- the shape of a servant API
data Req t = Req
  { reqMethod    :: Text
  , reqPathParts :: [Dynamic t (Either Text Text)]
  , qParams      :: [(Text, QueryPart t)]
  , reqBody      :: Maybe (Dynamic t (Either Text (BL.ByteString, Text)))
  , headers      :: [(Text, Dynamic t (Either Text Text))]
  , respHeaders  :: XhrResponseHeaders
  , authData     :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependToPathParts :: Dynamic t (Either Text Text) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => Text -> Dynamic t (Either Text a) -> Req t -> Req t
addHeader name val req = req { headers = (name, (fmap . fmap) (TE.decodeUtf8 . toHeader) val) : headers req }


-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

-- | This function actually performs the request.
performRequest :: forall t m.(SupportsServantReflex t m)
               => Text
               -> Req t
               -> Dynamic t BaseUrl
               -> Event t ()
               -> m (Event t XhrResponse, Event t Text)
performRequest reqMeth req reqHost trigger = do

  let t :: Dynamic t [Either Text Text]
      t = sequence $ reverse $ reqPathParts req

      baseUrl :: Dynamic t (Either Text Text)
      baseUrl = Right . showBaseUrl <$> reqHost

      urlParts :: Dynamic t (Either Text [Text])
      urlParts = fmap sequence t

      urlPath :: Dynamic t (Either Text Text)
      urlPath = (fmap.fmap) (T.intercalate "/") urlParts

      queryPartString :: (Text, QueryPart t) -> Dynamic t (Maybe (Either Text Text))
      queryPartString (pName, qp) = case qp of
        QueryPartParam p -> ffor p $ \case
          Left e         -> Just (Left e)
          Right (Just a) -> Just (Right $ pName <> "=" <> a)
          Right Nothing  -> Nothing
        QueryPartParams ps -> ffor ps $ \pStrings ->
          if null pStrings
          then Nothing
          else Just $ Right (T.intercalate "&" (fmap (\p -> pName <> "=" <> p) pStrings))
        QueryPartFlag fl -> ffor fl $ \case
          True ->  Just $ Right pName
          False -> Nothing


      queryPartStrings :: [Dynamic t (Maybe (Either Text Text))]
      queryPartStrings = map queryPartString (qParams req)
      queryPartStrings' = fmap (sequence . catMaybes) $ sequence queryPartStrings :: Dynamic t (Either Text [Text])
      queryString :: Dynamic t (Either Text Text) =
        ffor queryPartStrings' $ \qs -> fmap (T.intercalate "&") qs
--        ffor queryPartStrings' $ \qs -> fmap (T.intercalate "&") (sequence qs)
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </> if T.null q then p else p <> "?" <> q) baseUrl urlPath queryString
        where
          (</>) :: Text -> Text -> Text
          x </> y | ("/" `T.isSuffixOf` x) || ("/" `T.isPrefixOf` y) = x <> y
                  | otherwise = x <> "/" <> y


      xhrHeaders :: Dynamic t (Either Text [(Text, Text)])
      xhrHeaders = (fmap sequence . sequence . fmap f . headers) req
        where
          f = \(headerName, dynam) ->
                fmap (fmap (\rightVal -> (headerName, rightVal))) dynam

      mkConfigBody :: Either Text [(Text,Text)]
                   -> (Either Text (BL.ByteString, Text))
                   -> Either Text (XhrRequestConfig XhrPayload)
      mkConfigBody ehs rb = case (ehs, rb) of
                  (_, Left e)                     -> Left e
                  (Left e, _)                     -> Left e
                  (Right hs, Right (bBytes, bCT)) ->
                    Right $ XhrRequestConfig
                      { _xhrRequestConfig_sendData = bytesToPayload bBytes
                      , _xhrRequestConfig_headers  =
                                    Map.insert "Content-Type" bCT (Map.fromList hs)
                      , _xhrRequestConfig_user = Nothing
                      , _xhrRequestConfig_password = Nothing
                      , _xhrRequestConfig_responseType = Nothing
                      , _xhrRequestConfig_withCredentials = False
                      , _xhrRequestConfig_responseHeaders = def
                      }

      xhrOpts :: Dynamic t (Either Text (XhrRequestConfig XhrPayload))
      xhrOpts = case reqBody req of
        Nothing    -> ffor xhrHeaders $ \case
                               Left e -> Left e
                               Right hs -> Right $ def { _xhrRequestConfig_headers = Map.fromList hs
                                                       , _xhrRequestConfig_user = Nothing
                                                       , _xhrRequestConfig_password = Nothing
                                                       , _xhrRequestConfig_responseType = Nothing
                                                       , _xhrRequestConfig_sendData = ""
                                                       , _xhrRequestConfig_withCredentials = False
                                                       }
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody

      mkAuth :: Maybe BasicAuthData -> Either Text (XhrRequestConfig x) -> Either Text (XhrRequestConfig x)
      mkAuth _ (Left e) = Left e
      mkAuth Nothing r  = r
      mkAuth (Just (BasicAuthData u p)) (Right config) = Right $ config
        { _xhrRequestConfig_user     = Just $ TE.decodeUtf8 u
        , _xhrRequestConfig_password = Just $ TE.decodeUtf8 p}

      addAuth :: Dynamic t (Either Text (XhrRequestConfig x))
              -> Dynamic t (Either Text (XhrRequestConfig x))
      addAuth xhr = case authData req of
        Nothing -> xhr
        Just auth -> liftA2 mkAuth auth xhr

      xhrReq = (liftA2 . liftA2) (\p opt -> XhrRequest reqMeth p opt) xhrUrl (addAuth xhrOpts)

  let reqs    = tagPromptlyDyn xhrReq trigger
      okReqs  = fmapMaybe (either (const Nothing) Just) reqs
      badReqs = fmapMaybe (either Just (const Nothing)) reqs

  resps <- performRequestAsync okReqs

  return (resps, badReqs)

type XhrPayload = T.Text
bytesToPayload :: BL.ByteString -> XhrPayload
bytesToPayload = TE.decodeUtf8 . BL.toStrict

performRequestNoBody :: forall t m .(SupportsServantReflex t m)
                     => Text
                     -> Req t
                     -> Dynamic t BaseUrl
                     -> Event t () -> m (Event t (ReqResult NoContent))
performRequestNoBody reqMeth req reqHost trigger = do
  (resp, badReq) <- performRequest reqMeth req reqHost trigger
  return $ leftmost [ fmap (ResponseSuccess NoContent) resp, fmap RequestFailure badReq]


performRequestCT :: (SupportsServantReflex t m,
                     MimeUnrender ct a)
                 => Proxy ct -> Text -> Req t -> Dynamic t BaseUrl
                 -> Event t () -> m (Event t (ReqResult a))
performRequestCT ct reqMeth req reqHost trigger = do
  (resp, badReq) <- performRequest reqMeth req reqHost trigger
  let decodes = ffor resp $ \xhr ->
        ((mimeUnrender ct . BL.fromStrict . TE.encodeUtf8)
         =<< note "No body text" (_xhrResponse_responseText xhr), xhr)
      reqs = ffor decodes $ \case
        (Right a, r) -> ResponseSuccess a r
        (Left e,  r) -> ResponseFailure (T.pack e) r
  return $ leftmost [reqs, fmap RequestFailure badReq]

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
