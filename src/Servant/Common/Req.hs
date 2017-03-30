{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Servant.Common.Req where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Applicative        (liftA2, liftA3)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Functor.Compose
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Traversable           (forM)
import           Data.Typeable
import           Reflex.Dom                 hiding (tag)
import           Servant.Common.BaseUrl     (BaseUrl, showBaseUrl,
                                             SupportsServantReflex)
import           Servant.API.ContentTypes   (JSON, MimeUnrender(..),
                                             NoContent(..))
import           Web.HttpApiData            (ToHttpApiData(..))
-------------------------------------------------------------------------------
import           Servant.API.BasicAuth


#ifdef ghcjs_HOST_OS
import Control.Exception
import           Control.Monad (liftM)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Aeson                 as A
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Internal as T
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import GHCJS.Buffer (create, fromByteString, getArrayBuffer)
import GHCJS.Types (JSVal, jsval)
import GHCJS.Marshal (FromJSVal(..), fromJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import qualified JavaScript.Object.Internal as OI
import           Data.Scientific (Scientific, scientific, fromFloatDigits)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Internal
#endif


------------------------------------------------------------------------------
-- | The result of a request event
data ReqResult tag a
    = ResponseSuccess tag a XhrResponse
      -- ^ The succesfully decoded response from a request tagged with 'tag'
    | ResponseFailure tag Text XhrResponse
      -- ^ The failure response, which may have failed decoding or had
      --   a non-successful response code
    | RequestFailure  tag Text
      -- ^ A failure to construct the request tagged with 'tag' at trigger time
  deriving (Functor)


------------------------------------------------------------------------------
-- | Simple filter/accessor for successful responses, when you want to
-- ignore the error case. For example:
-- >> goodResponses <- fmapMaybe reqSuccess <$> clientFun triggers
reqSuccess :: ReqResult tag a -> Maybe a
reqSuccess (ResponseSuccess _ x _) = Just x
reqSuccess _                       = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor like 'reqSuccess', but keeping the request tag
reqSuccess' :: ReqResult tag a -> Maybe (tag,a)
reqSuccess' (ResponseSuccess tag x _) = Just (tag,x)
reqSuccess' _                         = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor for any failure case
reqFailure :: ReqResult tag a -> Maybe Text
reqFailure (ResponseFailure _ s _) = Just s
reqFailure (RequestFailure  _ s)   = Just s
reqFailure _                       = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor for the raw XHR response
response :: ReqResult tag a -> Maybe XhrResponse
response (ResponseSuccess _ _ x) = Just x
response (ResponseFailure _ _ x) = Just x
response _                       = Nothing

------------------------------------------------------------------------------
-- | Retrieve response tag
reqTag :: ReqResult tag a -> tag
reqTag (ResponseSuccess tag _ _) = tag
reqTag (ResponseFailure tag _ _) = tag
reqTag (RequestFailure  tag _  ) = tag

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


reqToReflexRequest
    :: forall t. Reflex t
    => Text
    -> Dynamic t BaseUrl
    -> Req t
    -> (Dynamic t (Either Text (XhrRequest XhrPayload)))
reqToReflexRequest reqMeth reqHost req =
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
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </> if T.null q then p else p <> "?" <> q)
          baseUrl urlPath queryString
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

  in xhrReq

-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

-- | This function actually performs the request.
performRequests :: forall t m f tag.(SupportsServantReflex t m, Traversable f)
                => Text
                -> Dynamic t (f (Req t))
                -> Dynamic t BaseUrl
                -> Event t tag
                -> m (Event t (tag, f (Either Text XhrResponse)))
performRequests reqMeth rs reqHost trigger = do
  -- let xhrReqs = sequence $ (\r -> reqToReflexRequest reqMeth r reqHost) <$> rs :: Dynamic t (f (Either Text (XhrRequest XhrPayload)))
  let xhrReqs = join $ (\(fxhr :: f (Req t)) -> sequence $ reqToReflexRequest reqMeth reqHost <$> fxhr) <$> rs
  let reqs    = attachPromptlyDynWith (\fxhr t -> Compose (t, fxhr)) xhrReqs trigger
  resps <- performSomeRequestsAsync reqs
  return $ getCompose <$> resps

-- | Issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performSomeRequestsAsync
    :: (MonadIO (Performable m),
        HasWebView (Performable m),
        PerformEvent t m,
        TriggerEvent t m,
        Traversable f,
        IsXhrPayload a)
    => Event t (f (Either Text (XhrRequest a)))
    -> m (Event t (f (Either Text XhrResponse)))
performSomeRequestsAsync = performSomeRequestsAsync' newXMLHttpRequest . fmap return


------------------------------------------------------------------------------
-- | A modified version or Reflex.Dom.Xhr.performRequestsAsync
-- that accepts 'f (Either e (XhrRequestb))' events
performSomeRequestsAsync'
    :: (MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f)
    => (XhrRequest b -> (a -> IO ()) -> Performable m XMLHttpRequest)
    -> Event t (Performable m (f (Either Text (XhrRequest b)))) -> m (Event t (f (Either Text a)))
performSomeRequestsAsync' newXhr req = performEventAsync $ ffor req $ \hrs cb -> do
  rs <- hrs
  resps <- forM rs $ \r -> case r of
      Left e -> do
          resp <- liftIO $ newMVar (Left e)
          return resp
      Right r' -> do
          resp <- liftIO newEmptyMVar
          _ <- newXhr r' $ liftIO . putMVar resp . Right
          return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()



-- | This function actually performs the request.
performRequest :: forall t m tag .(SupportsServantReflex t m)
               => Text
               -> (Req t)
               -> Dynamic t BaseUrl
               -> Event t tag
               -> m (Event t (tag, XhrResponse), Event t (tag, Text))
performRequest reqMeth req reqHost trigger = do

  let xhrReq  = reqToReflexRequest reqMeth reqHost req
  let reqs    = attachPromptlyDynWith (flip (,)) xhrReq trigger
      okReqs  = fmapMaybe (\(t,e) -> either (const Nothing) (Just . (t,)) e) reqs
      badReqs = fmapMaybe (\(t,e) -> either (Just . (t,)) (const Nothing) e) reqs

  resps <- performRequestsAsync okReqs

  return (resps, badReqs)


type XhrPayload = T.Text
bytesToPayload :: BL.ByteString -> XhrPayload
bytesToPayload = TE.decodeUtf8 . BL.toStrict


-- performRequestNoBody :: forall t m tag.(SupportsServantReflex t m)
--                      => Text
--                      -> Req t
--                      -> Dynamic t BaseUrl
--                      -> Event t tag -> m (Event t (ReqResult tag NoContent))
-- performRequestNoBody reqMeth req reqHost trigger = do
--   (resp, badReq) <- performRequest reqMeth req reqHost trigger
--   let decodeResp = const $ Right NoContent
--   return $ leftmost [ fmap (evalResponse decodeResp) resp
--                     , fmap (uncurry RequestFailure) badReq
--                     ]


-- performRequestCT
--     :: (SupportsServantReflex t m,
--         MimeUnrender ct a)
--     => Proxy ct
--     -> Text
--     -> Req t
--     -> Dynamic t BaseUrl
--     -> Event t tag
--     -> m (Event t (ReqResult tag a))
-- performRequestCT ct reqMeth req reqHost trigger = do
--   (resp, badReq) <- performRequest reqMeth req reqHost trigger
--   let decodeResp x = first T.pack .
--                      mimeUnrender ct .
--                      BL.fromStrict .
--                      TE.encodeUtf8 =<< note "No body text"
--                      (_xhrResponse_responseText x)

--   return $ leftmost [fmap (evalResponse decodeResp) resp
--                     , fmap (uncurry RequestFailure) badReq
--                     ]


performRequestsCT
    :: (SupportsServantReflex t m,
        MimeUnrender ct a, Traversable f)
    => Proxy ct
    -> Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> Event t tag
    -> m (Event t (f (ReqResult tag a)))
performRequestsCT ct reqMeth reqs reqHost trigger = do
  resps <- performRequests reqMeth reqs reqHost trigger
  let decodeResp x = first T.pack .
                     mimeUnrender ct .
                     BL.fromStrict .
                     TE.encodeUtf8 =<< note "No body text"
                     (_xhrResponse_responseText x)
  return $ fmap
      (\(t,rs) -> ffor rs $ \r -> case r of
              Left e  -> RequestFailure t e
              Right g -> evalResponse decodeResp (t,g)
      )
      resps

mimeUnrender' :: (MimeUnrender ct a, Typeable ct, FromJSON a) => Proxy ct -> BS.ByteString -> Either String a
mimeUnrender' p bs
    | cantRawJson  = mimeUnrender p (BL.fromStrict bs)
    | otherwise    = note "Raw JSON decode failure" $ rawDecode bs
  where
      cantRawJson = typeOf (Proxy :: Proxy JSON) /= typeOf p

-- copied from http://lpaste.net/6122658287709061120
#ifdef ghcjs_HOST_OS
foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> JSVal
foreign import javascript unsafe "JSON['parse']($1)" js_jsonParse :: JSVal -> JSVal
rawDecode :: (FromJSON a) => BS.ByteString -> Maybe a
rawDecode bs = do
  -- Below copied from Reflex.Dom.WebSocket.Foreign
  let (b, off, len) = fromByteString bs
      -- x = return $ js_dataView off len  $ jsval $ getArrayBuffer b :: _
  let jsv = if BS.length bs == 0
            then unsafePerformIO $ jsval . getArrayBuffer <$> create 0
            else js_dataView off len $ jsval $ getArrayBuffer b

  -- let jsv = _ bs
  -- TODO pFromJSVal to avoid unsafePerformIO
  let res = unsafePerformIO $ try $ aesonFromJSVal $ js_jsonParse jsv
  case res of
    Left (_e :: SomeException) -> Nothing
    Right (v :: (Maybe Aeson.Value)) -> maybe Nothing go v
  where
    go v = case Aeson.fromJSON v of
      Aeson.Success a -> Just a
      _ -> Nothing


-- copied from http://lpaste.net/raw/353535  Thanks ncl28!
aesonFromJSVal :: JSVal -> IO (Maybe A.Value)
aesonFromJSVal r = case jsonTypeOf r of
    JSONNull    -> return (Just A.Null)
    JSONInteger -> liftM (A.Number . flip scientific 0 . (toInteger :: Int -> Integer))
         <$> fromJSVal r
    JSONFloat   -> liftM (A.Number . (fromFloatDigits :: Double -> Scientific))
         <$> fromJSVal r
    JSONBool    -> liftM A.Bool  <$> fromJSVal r
    JSONString  -> liftM A.String <$> fromJSVal r
    -- JSONArray   -> liftM (A.Array . V.fromList) <$> (fromJSVal r :: _)
    JSONArray   -> liftM (A.Array . V.fromList) <$>
                   runMaybeT (traverse (MaybeT . aesonFromJSVal) =<<
                    (MaybeT (fromJSVal r)))
    JSONObject  -> do
        props <- OI.listProps (OI.Object r)
        runMaybeT $ do
            propVals <- forM props $ \p -> do
                v <- MaybeT (aesonFromJSVal =<< OI.getProp p (OI.Object r))
                -- return (JSS.textFromJSString p, v)
                return (T.Text p, v)
            return (A.Object (H.fromList propVals))
{-# INLINE aesonFromJSVal #-}

#else
rawDecode :: FromJSON a => BS.ByteString -> Maybe a
rawDecode = decode . BL.fromStrict
#endif

performRequestsNoBody
    :: (SupportsServantReflex t m,
        Traversable f)
    => Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> Event t tag
    -> m (Event t (f (ReqResult tag NoContent)))
performRequestsNoBody reqMeth reqs reqHost trigger = do
  resps <- performRequests reqMeth reqs reqHost trigger
  let decodeResp = const $ Right NoContent
  return $ ffor resps $ \(tag,rs) -> ffor rs $ \r -> case r of
      Left e  -> RequestFailure tag e
      Right g -> evalResponse decodeResp (tag,g)


------------------------------------------------------------------------------
evalResponse
    :: (XhrResponse -> Either Text a)
    -> (tag, XhrResponse)
    -> ReqResult tag a
evalResponse decodeFun (tag, xhr) =
    let okStatus   = _xhrResponse_status xhr < 400
        errMsg = fromMaybe
            ("Empty response with error code " <>
                T.pack (show $ _xhrResponse_status xhr))
            (_xhrResponse_responseText xhr)
        respPayld  = if okStatus
                     then either
                          (\e -> ResponseFailure tag e xhr)
                          (\v -> ResponseSuccess tag v xhr)
                          (decodeFun xhr)
                     else ResponseFailure tag errMsg xhr
    in respPayld




note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

fmapL :: (e -> e') -> Either e a -> Either e' a
fmapL _ (Right a) = Right a
fmapL f (Left e)  = Left (f e)
