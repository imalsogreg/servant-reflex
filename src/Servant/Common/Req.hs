{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Servant.Common.Req where

import Control.Applicative (liftA2, liftA3)
import Data.ByteString.Char8 hiding (pack, filter, map, null, elem)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import qualified Data.Text.Encoding as TE
-- import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Proxy
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

-- import qualified Network.HTTP.Client as Client

import Web.HttpApiData

data ReqResult a = ResponseSuccess a XhrResponse
                 | ResponseFailure String XhrResponse
                 | RequestFailure String

reqSuccess :: ReqResult a -> Maybe a
reqSuccess (ResponseSuccess x _) = Just x
reqSuccess _                     = Nothing

reqFailure :: ReqResult a -> Maybe String
reqFailure (ResponseFailure s _) = Just s
reqFailure (RequestFailure s)    = Just s
reqFailure _                     = Nothing

response :: ReqResult a -> Maybe XhrResponse
response (ResponseSuccess _ x) = Just x
response (ResponseFailure _ x) = Just x
response _                     = Nothing

data QueryPart t = QueryPartParam  (Behavior t (Either String String))
                 | QueryPartParams (Behavior t [String])
                 | QueryPartFlag   (Behavior t Bool)

data Req t = Req
  { reqMethod    :: String
  , reqPathParts :: [Behavior t (Either String String)]
  , qParams      :: [(String, QueryPart t)]
  , reqBody      :: Maybe (Behavior t (Either String (BL.ByteString, String)))
  -- , reqAccept    :: [MediaType]  -- TODO ?
  , headers      :: [(String, Behavior t String)]
  }

defReq :: Reflex t => Req t
defReq = Req "GET" [] [] Nothing []

prependToPathParts :: Reflex t => Behavior t (Either String String) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => String -> Behavior t (Either String a) -> Req t -> Req t
addHeader name val req = req { headers = headers req
                                         ++ [(name, fmap (unpack . toHeader) val)]
--                                      ++ [(name, (fmap . fmap) (decodeUtf8 . toHeader) val)]
                             }

-- * performing requests

displayHttpRequest :: String -> String
displayHttpRequest httpmethod = "HTTP " ++ httpmethod ++ " request"

-- | This function actually performs the request.
performRequest :: forall t m.MonadWidget t m
               => String
               -> Req t
               -> Dynamic t BaseUrl
               -> Event t ()
               -> m (Event t XhrResponse, Event t String)
               -- -> ExceptT ServantError IO ( Int, ByteString, MediaType
               --                            , [HTTP.Header], Response ByteString)
performRequest reqMethod req reqHost trigger = do

  -- Ridiculous functor-juggling! How to clean this up?
  let t :: Behavior t [Either String String]
      t = sequence $ L.reverse $ reqPathParts req

      baseUrl :: Behavior t (Either String String)
      baseUrl = Right . showBaseUrl <$> current reqHost

      urlParts :: Behavior t (Either String [String])
      urlParts = fmap sequence t

      urlPath :: Behavior t (Either String String)
      urlPath = (fmap.fmap) (L.intercalate "/") urlParts

      queryPartString :: (String, QueryPart t) -> Behavior t (Maybe (Either String String))
      queryPartString (pName, qp) = case qp of
        QueryPartParam p -> ffor p $ \case
          Left e  -> Just (Left e)
          Right a -> Just (Right $ pName ++ "=" ++ a)
        QueryPartParams ps -> ffor ps $ \pStrings ->
          if null pStrings
          then Nothing
          else Just $ Right (L.intercalate "&" (fmap (\p -> pName ++ '=' : p) pStrings))
        QueryPartFlag fl -> ffor fl $ \case
          True ->  Just $ Right pName
          False -> Nothing


      queryPartStrings :: [Behavior t (Maybe (Either String String))]
      queryPartStrings = map queryPartString (qParams req)
      queryPartStrings' = fmap (sequence . catMaybes) $ sequence queryPartStrings :: Behavior t (Either String [String])
      queryString :: Behavior t (Either String String) =
        ffor queryPartStrings' $ \qs -> fmap (L.intercalate "&") qs
--        ffor queryPartStrings' $ \qs -> fmap (L.intercalate "&") (sequence qs)
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </>  if null q then p else p ++ '?' : q) baseUrl urlPath queryString
        where
          (</>) :: String -> String -> String
          x </> y | ("/" `L.isSuffixOf` x) || ("/" `L.isPrefixOf` y) = x ++ y
                  | otherwise = x ++ '/':y

      xhrHeaders :: Behavior t [(String, String)]
      xhrHeaders = sequence $ ffor (headers req) $ \(hName, hVal) -> fmap (hName,) hVal


      mkConfigBody :: [(String,String)] -> (Either String (BL.ByteString, String)) -> Either String XhrRequestConfig
      mkConfigBody hs rb = case rb of
                  Left e               -> Left e
                  (Right (bBytes, bCT)) ->
                    Right $ def { _xhrRequestConfig_sendData = Just (BL.unpack bBytes)
                                , _xhrRequestConfig_headers  =
                                    Map.insert "Content-Type" bCT (_xhrRequestConfig_headers def)}

      xhrOpts :: Behavior t (Either String XhrRequestConfig)
      xhrOpts = case reqBody req of
        Nothing    -> fmap (\h -> Right $ def { _xhrRequestConfig_headers = Map.fromList h }) xhrHeaders
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody
      xhrReq = (liftA2 . liftA2) (\p opt -> XhrRequest reqMethod p opt) xhrUrl xhrOpts

  let reqs    = tag xhrReq trigger
      okReqs  = fmapMaybe (either (const Nothing) Just) reqs
      badReqs = fmapMaybe (either Just (const Nothing)) reqs

  resps <- performRequestAsync okReqs
  return (resps, badReqs)

  -- let oneNamedPair :: String -> [QueryPart] -> String
  --     oneNamedPair pName ps =
  --       L.intercalate "&" $ ffor ps $ \case
  --         QueryPartParam pval -> pName ++ "=" ++ pval
  --         QueryPartFlag True  -> pName
  --         QueryPartFlag False -> error "Impossible case"

  --     t' :: [Behavior t String]
  --     t' = map (\(pName, pVals) -> fmap (oneNamedPair pName) pVals)
  --           (qParams req)

  --     queryString :: Behavior t String
  --     queryString = fmap (L.intercalate "&") (sequence t')

  --     xhrUrl = (liftA2 . liftA2) (\u q -> u ++ '?' : q) urlPath (fmap Just queryString)

-- TODO implement
-- => String -> Req -> BaseUrl -> ExceptT ServantError IO [HTTP.Header]
  -- TODO Proxy probably not needed
performRequestNoBody ::
  forall t m .MonadWidget t m => String -> Req t -> Dynamic t BaseUrl
--                               -> Event t () -> m (Event t (Maybe NoContent, XhrResponse))
                              -> Event t () -> m (Event t (ReqResult NoContent))
performRequestNoBody reqMethod req reqHost trigger = do
  -- performRequest reqMethod req reqHost trigger
  undefined
  -- return hdrs

performRequestCT :: (MonadWidget t m, MimeUnrender ct a)
                 => Proxy ct -> String -> Req t -> Dynamic t BaseUrl
                 -> Event t () -> m (Event t (ReqResult a))
performRequestCT ct reqMethod req reqHost trigger = do
  (resp, badReq) <- performRequest reqMethod req reqHost trigger
  let decodes = ffor resp $ \xhr ->
        ((mimeUnrender ct . BL.fromStrict . TE.encodeUtf8)
         =<< note "No body text" (_xhrResponse_responseText xhr), xhr)
      reqs = ffor decodes $ \case
        (Right a, resp) -> ResponseSuccess a resp
        (Left e,  resp) -> ResponseFailure e resp
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
--                                          , uriPort = ":" ++ show reqPort
--                                          }
--                              , uriPath = path ++ reqPath req
--                              }

--         setrqb r = case reqBody req of
--                      Nothing -> r
--                      Just (b,t) -> r { requestBody = RequestBodyLBS b
--                                      , requestHeaders = requestHeaders r
--                                                      ++ [(hContentType, cs . show $ t)] }
--         setQS = setQueryString $ queryTextToQuery (qs req)
--         setheaders r = r { requestHeaders = requestHeaders r
--                                          <> fmap toProperHeader (headers req) }
--         setAccept r = r { requestHeaders = filter ((/= "Accept") . fst) (requestHeaders r)
--                                         <> [("Accept", renderHeader $ reqAccept req)
--                                               | not . null . reqAccept $ req] }
--         toProperHeader (name, val) =
--           (fromString name, encodeUtf8 val)
