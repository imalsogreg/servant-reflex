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

data ReqResult e a = ResponseSuccess a XhrResponse
                   | ResponseFailure String XhrResponse
                   | RequestFailure e

-- data ServantError
--   = FailureResponse
--     { responseStatus            :: Status
--     , responseContentType       :: MediaType
--     , responseBody              :: ByteString
--     }
--   | DecodeFailure
--     { decodeError               :: String
--     , responseContentType       :: MediaType
--     , responseBody              :: ByteString
--     }
--   | UnsupportedContentType
--     { responseContentType       :: MediaType
--     , responseBody              :: ByteString
--     }
--   | InvalidContentTypeHeader
--     { responseContentTypeHeader :: ByteString
--     , responseBody              :: ByteString
--     }
--   | ConnectionError
--     { connectionError           :: SomeException
--     }
--   deriving (Show, Typeable)

-- instance Exception ServantError

data QueryPart t = QueryPartParam (Behavior t [String])
                 | QueryPartFlag  (Behavior t Bool)

data Req t = Req
  { reqMethod    :: String
  , reqPathParts :: [Behavior t (Maybe String)]
  , qParams      :: [(String, QueryPart t)]
  , reqBody      :: Maybe (Behavior t (Maybe (BL.ByteString, String)))
  -- , reqAccept    :: [MediaType]  -- TODO ?
  , headers      :: [(String, Behavior t String)]
  }

defReq :: Reflex t => Req t
defReq = Req "GET" [] [] Nothing []

prependToPathParts :: Reflex t => Behavior t (Maybe String) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => String -> Behavior t (Maybe a) -> Req t -> Req t
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
               -> m (Event t XhrResponse)
               -- -> ExceptT ServantError IO ( Int, ByteString, MediaType
               --                            , [HTTP.Header], Response ByteString)
performRequest reqMethod req reqHost trigger = do

  -- Ridiculous functor-juggling! How to clean this up?
  let t :: Behavior t [Maybe String]
      t = sequence $ reqPathParts req

      baseUrl :: Behavior t (Maybe String)
      baseUrl = Just . showBaseUrl <$> current reqHost

      urlParts :: Behavior t (Maybe [String])
      urlParts = fmap sequence t

      urlPath :: Behavior t (Maybe String)
      urlPath = (fmap.fmap) (L.intercalate "/") urlParts

      queryPartString :: (String, QueryPart t) -> Behavior t (Maybe String)
      queryPartString (pName, qp) = case qp of
        QueryPartParam ps -> ffor ps $ \pStrings -> -- case null pStrings of
          if null pStrings
          then Nothing
          else Just (L.intercalate "&" (fmap (\p -> pName ++ '=' : p) pStrings))
        QueryPartFlag fl -> ffor fl $ \case
          True ->  Just pName
          False -> Nothing


      queryPartStrings = map queryPartString (qParams req)
      queryPartStrings' = sequence queryPartStrings :: Behavior t [Maybe String]
      queryString :: Behavior t (Maybe String) =
        ffor queryPartStrings' $ \qs -> Just (L.intercalate "&" (catMaybes qs))
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </>  if null q then p else p ++ '?' : q) baseUrl urlPath queryString
        where
          (</>) :: String -> String -> String
          x </> y | ("/" `L.isSuffixOf` x) || ("/" `L.isPrefixOf` y) = x ++ y
                  | otherwise = x ++ '/':y

      xhrHeaders :: Behavior t [(String, String)]
      xhrHeaders = sequence $ ffor (headers req) $ \(hName, hVal) -> fmap (hName,) hVal


      mkConfigBody :: [(String,String)] -> (Maybe (BL.ByteString, String)) -> Maybe XhrRequestConfig
      mkConfigBody hs rb = case rb of
                  Nothing              -> Nothing
                  (Just (bBytes, bCT)) ->
                    Just $ def { _xhrRequestConfig_sendData = Just (BL.unpack bBytes)
                               , _xhrRequestConfig_headers  =
                                   Map.insert "Content-Type" bCT (_xhrRequestConfig_headers def)}

      xhrOpts :: Behavior t (Maybe XhrRequestConfig)
      xhrOpts = case reqBody req of
        Nothing    -> fmap (\h -> Just $ def { _xhrRequestConfig_headers = Map.fromList h }) xhrHeaders
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody
      xhrReq = (liftA2 . liftA2) (\p opt -> XhrRequest reqMethod p opt) xhrUrl xhrOpts

  performRequestAsync (fmapMaybe id $ tag xhrReq trigger)

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
                              -> Event t () -> m (Event t (Maybe NoContent, XhrResponse))
performRequestNoBody reqMethod req reqHost trigger = do
  -- performRequest reqMethod req reqHost trigger
  undefined
  -- return hdrs

performRequestCT :: (MonadWidget t m, MimeUnrender ct a)
                 => Proxy ct -> String -> Req t -> Dynamic t BaseUrl
                 -> Event t () -> m (Event t (ReqResult e a))
performRequestCT ct reqMethod req reqHost trigger = do
  resp <- performRequest reqMethod req reqHost trigger
  let decodes = ffor resp $ \xhr ->
        ((mimeUnrender ct . BL.fromStrict . TE.encodeUtf8)
         =<< note "No body text" (_xhrResponse_responseText xhr), xhr)
  return $ ffor decodes $ \case
    (Right a, resp) -> ResponseSuccess a resp
    (Left e,  resp) -> ResponseFailure e resp

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
