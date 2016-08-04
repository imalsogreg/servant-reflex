{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Servant.Common.Req where

import Control.Applicative (liftA2, liftA3)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
-- import qualified Data.Foldable as F
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Map as Map
-- import Data.Monoid
-- import Data.String
-- import Data.String.Conversions
-- import Data.Proxy
-- import Data.Text (Text)
-- import Data.Text.Encoding
-- import Data.Typeable
-- import Network.HTTP.Client hiding (Proxy, path)
-- import Network.HTTP.Media
-- import Network.HTTP.Types
-- import qualified Network.HTTP.Types.Header   as HTTP
-- import Network.URI hiding (path)
-- import Servant.API.ContentTypes
import Servant.Common.BaseUrl
import Servant.API.ContentTypes
import Reflex
import Reflex.Dom

import Servant.API.BasicAuth

-- import qualified Network.HTTP.Client as Client

import Web.HttpApiData

data ReqResult a = ResponseSuccess a XhrResponse
                 | ResponseFailure Text XhrResponse
                 | RequestFailure Text

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


data QParam a = QParamSome a | QNone | QParamInvalid Text

qParamToQueryPart :: ToHttpApiData a => QParam a -> Either Text (Maybe Text)
qParamToQueryPart (QParamSome a)    = Right (Just $ toQueryParam a)
qParamToQueryPart QNone             = Right Nothing
qParamToQueryPart (QParamInvalid e) = Left e

data QueryPart t = QueryPartParam  (Dynamic t (Either Text (Maybe Text)))
                 | QueryPartParams (Dynamic t [Text])
                 | QueryPartFlag   (Dynamic t Bool)

data Req t = Req
  { reqMethod    :: Text
  , reqPathParts :: [Dynamic t (Either Text Text)]
  , qParams      :: [(Text, QueryPart t)]
  , reqBody      :: Maybe (Dynamic t (Either Text (BL.ByteString, Text)))
  -- , reqAccept    :: [MediaType]  -- TODO ?
  , headers      :: [(Text, Dynamic t Text)]
  , authData     :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Reflex t => Req t
defReq = Req "GET" [] [] Nothing [] Nothing

prependToPathParts :: Reflex t => Dynamic t (Either Text Text) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => Text -> Dynamic t (Either Text a) -> Req t -> Req t
addHeader name val req = req { headers = headers req
                                         <> [(name, fmap (TE.decodeUtf8 . toHeader) val)]
--                                      <> [(name, (fmap . fmap) (decodeUtf8 . toHeader) val)]
                             }

-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

-- | This function actually performs the request.
performRequest :: forall t m.MonadWidget t m
               => Text
               -> Req t
               -> Dynamic t BaseUrl
               -> Event t ()
               -> m (Event t XhrResponse, Event t Text)
               -- -> ExceptT ServantError IO ( Int, ByteString, MediaType
               --                            , [HTTP.Header], Response ByteString)
performRequest reqMeth req reqHost trigger = do

  -- Ridiculous functor-juggling! How to clean this up?
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

      xhrHeaders :: Dynamic t [(Text, Text)]
      xhrHeaders = sequence $ ffor (headers req) $ \(hName, hVal) -> fmap (hName,) hVal

      mkConfigBody :: [(Text,Text)] -> (Either Text (BL.ByteString, Text)) -> Either Text (XhrRequestConfig LT.Text)
      mkConfigBody _ rb = case rb of
                  Left e               -> Left e
                  (Right (bBytes, bCT)) ->
                    Right $ def { _xhrRequestConfig_sendData = LTE.decodeUtf8 bBytes
                                , _xhrRequestConfig_headers  =
                                    Map.insert "Content-Type" bCT (_xhrRequestConfig_headers def)}

      xhrOpts :: Dynamic t (Either Text (XhrRequestConfig LT.Text))
      xhrOpts = case reqBody req of
        Nothing    -> fmap (\h -> Right $ XhrRequestConfig
                        (Map.fromList h) Nothing Nothing Nothing "") xhrHeaders
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody

      mkAuth :: Maybe BasicAuthData -> Either Text (XhrRequestConfig x) -> Either Text (XhrRequestConfig x)
      mkAuth _ (Left e) = Left e
      mkAuth Nothing r  = r
      mkAuth (Just (BasicAuthData u p)) (Right config) = Right $ config
        { _xhrRequestConfig_user     = Just $ TE.decodeUtf8 u
        , _xhrRequestConfig_password = Just $ TE.decodeUtf8 p}

      addAuth :: Dynamic t (Either Text (XhrRequestConfig x)) -> Dynamic t (Either Text (XhrRequestConfig x))
      addAuth xhr = case authData req of
        Nothing -> xhr
        Just auth -> liftA2 mkAuth auth xhr

      xhrReq = (liftA2 . liftA2) (\p opt -> XhrRequest reqMeth p opt) xhrUrl (addAuth xhrOpts)

  let reqs    = tagDyn xhrReq trigger
      okReqs  = fmapMaybe (either (const Nothing) Just) reqs
      badReqs = fmapMaybe (either Just (const Nothing)) reqs

#ifndef ghcjs_HOST_OS
  resps <- performRequestAsync (fmap LT.toStrict <$> okReqs)
#else
  resps <- performRequestAsync (fmap LT.unpack <$> okReqs)
#endif

  return (resps, badReqs)

  -- let oneNamedPair :: String -> [QueryPart] -> String
  --     oneNamedPair pName ps =
  --       T.intercalate "&" $ ffor ps $ \case
  --         QueryPartParam pval -> pName <> "=" <> pval
  --         QueryPartFlag True  -> pName
  --         QueryPartFlag False -> error "Impossible case"

  --     t' :: [Behavior t String]
  --     t' = map (\(pName, pVals) -> fmap (oneNamedPair pName) pVals)
  --           (qParams req)

  --     queryString :: Behavior t String
  --     queryString = fmap (T.intercalate "&") (sequence t')

  --     xhrUrl = (liftA2 . liftA2) (\u q -> u <> '?' : q) urlPath (fmap Just queryString)

-- TODO implement
-- => String -> Req -> BaseUrl -> ExceptT ServantError IO [HTTP.Header]
  -- TODO Proxy probably not needed
performRequestNoBody ::
  forall t m .MonadWidget t m => Text -> Req t -> Dynamic t BaseUrl
--                               -> Event t () -> m (Event t (Maybe NoContent, XhrResponse))
                              -> Event t () -> m (Event t (ReqResult NoContent))
performRequestNoBody _ _ _ _ = do
  -- performRequest reqMeth req reqHost trigger
  undefined
  -- return hdrs

performRequestCT :: (MonadWidget t m, MimeUnrender ct a)
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


  -- partialRequest <- liftIO $ reqToRequest req reqHost

  -- let request = partialRequest { Client.method = reqMethod
  --                              , checkStatus = \ _status _headers _cookies -> Nothing
  -- , qs           :: QueryText
  -- , qs           :: QueryText
  --                              }

  -- eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request manager
  -- case eResponse of
  --   Left err ->
  --     throwE . ConnectionError $ SomeException err

  --   Right response -> do
  --     let status = Client.responseStatus response
  --         body = Client.responseBody response
  --         hdrs = Client.responseHeaders response
  --         status_code = statusCode status
  --     ct <- case lookup "Content-Type" $ Client.responseHeaders response of
  --                Nothing -> pure $ "application/octet-stream"
  --                Just t -> case parseAccept t of
  --                  Nothing -> throwE $ InvalidContentTypeHeader (cs t) body
  --                  Just t' -> pure t'
  --     unless (status_code >= 200 && status_code < 300) $
  --       throwE $ FailureResponse status ct body
  --     return (status_code, body, ct, hdrs, response)


-- TODO implement
-- performRequestCT :: (MimeUnrender ct result, Reflex t) =>
--   Proxy ct -> String -> Req t -> BaseUrl -> Event t ()
--     -> m (Event t (Maybe result, XhrResponse)) -- ExceptT ServantError IO ([HTTP.Header], result)
-- performRequestCT ct reqMethod req reqHost = do
--   let acceptCT = contentType ct
--   (_status, respBody, respCT, hdrs, _response) <-
--     performRequest reqMethod (req { reqAccept = [acceptCT] }) reqHost manager
--   unless (matches respCT (acceptCT)) $ throwE $ UnsupportedContentType respCT respBody
--   case mimeUnrender ct respBody of
--     Left err -> throwE $ DecodeFailure err respCT respBody
--     Right val -> return (hdrs, val)

-- catchConnectionError :: IO a -> IO (Either ServantError a)
-- catchConnectionError action =
--   catch (Right <$> action) $ \e ->
--     pure . Left . ConnectionError $ SomeException (e :: HttpException)

-- setRQBody :: ByteString -> String -> Req -> Req
-- setRQBody b t req = req { reqBody = Just (b, t) }

-- TODO: Helpful soon!
-- reqToRequest :: (Reflex t, Functor m, MonadThrow m) => Req t -> BaseUrl -> m Request
-- reqToRequest req (BaseUrl reqScheme reqHost reqPort path) =
--     setheaders . setAccept . setrqb . setQS <$> parseUrl url

--   where url = show $ nullURI { uriScheme = case reqScheme of
--                                   Http  -> "http:"
--                                   Https -> "https:"
--                              , uriAuthority = Just $
--                                  URIAuth { uriUserInfo = ""
--                                          , uriRegName = reqHost
--                                          , uriPort = ":" <> show reqPort
--                                          }
--                              , uriPath = path <> reqPath req
--                              }

--         setrqb r = case reqBody req of
--                      Nothing -> r
--                      Just (b,t) -> r { requestBody = RequestBodyLBS b
--                                      , requestHeaders = requestHeaders r
--                                                      <> [(hContentType, cs . show $ t)] }
--         setQS = setQueryString $ queryTextToQuery (qs req)
--         setheaders r = r { requestHeaders = requestHeaders r
--                                          <> fmap toProperHeader (headers req) }
--         setAccept r = r { requestHeaders = filter ((/= "Accept") . fst) (requestHeaders r)
--                                         <> [("Accept", renderHeader $ reqAccept req)
--                                               | not . null . reqAccept $ req] }
--         toProperHeader (name, val) =
--           (fromString name, encodeUtf8 val)
