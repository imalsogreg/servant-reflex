{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Common.Req where

-------------------------------------------------------------------------------
import           Control.Applicative               (liftA2, liftA3)
import           Control.Arrow                     ((&&&))
import           Control.Concurrent
import           Control.Monad                     (join)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Aeson
import           Data.Bifunctor                    (first)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as Builder
import           Data.ByteString.Lazy              (ByteString)
import qualified Data.ByteString.Lazy.Char8        as BL
import           Data.Functor.Compose
import qualified Data.JSString.Text                as JS (lazyTextFromJSString)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.Monoid                       ((<>))
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import qualified Data.Text.Lazy                    as TL
import           Data.Traversable                  (forM)
import qualified Language.Javascript.JSaddle       as JS
import           Language.Javascript.JSaddle.Monad (JSM, MonadJSM, liftJSM)
import qualified Network.URI                       as N
import           Reflex.Dom.Core                   hiding (tag)
import           Servant.API.ContentTypes          (Accept, FormUrlEncoded,
                                                    JSON, NoContent (..),
                                                    OctetStream, PlainText)
import           Servant.Common.BaseUrl            (BaseUrl,
                                                    SupportsServantReflex,
                                                    showBaseUrl)
import           Web.FormUrlEncoded                (FromForm, urlDecodeAsForm)
import           Web.HttpApiData                   (ToHttpApiData (..))
-------------------------------------------------------------------------------
import           Servant.API.BasicAuth
-------------------------------------------------------------------------------

-- #ifndef __GHCJS__
-- jsonDecode :: JS.JSString -> Maybe a
-- jsonDecode = error "jsonDecode is only available in GHCJS"
-- #endif

-----------------------------------------------------------------------------
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

data ClientOptions = ClientOptions
    { optsRequestFixup :: forall a. Show a => XhrRequest a -> JSM (XhrRequest a)
      -- ^ Aribtrarily modify requests just before they are sent.
      -- Warning: This escape hatch opens the possibility for your
      -- requests to diverge from what the server expects, when the
      -- server is also derived from a servant API
    }

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions { optsRequestFixup = return }

-- withCredentials :: Lens' (XhrRequest a) Bool
withCredentials :: (Show a, Functor f) => (Bool -> f Bool) -> XhrRequest a -> f (XhrRequest a)
withCredentials inj r@(XhrRequest _ _ cfg) =
    let cfg' = (\b -> cfg { _xhrRequestConfig_withCredentials = b}) <$>
               inj (_xhrRequestConfig_withCredentials cfg)
    in (\c' -> r {_xhrRequest_config = c' }) <$> cfg'

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
-- | Simple filter/accessor like 'reqFailure', but keeping the request tag
reqFailure' :: ReqResult tag a -> Maybe (tag,Text)
reqFailure' (ResponseFailure tag s _) = Just (tag,s)
reqFailure' (RequestFailure  tag s)   = Just (tag,s)
reqFailure' _                         = Nothing


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
data Req t = forall o. (IsXhrPayload o, Show o) => Req
  { reqMethod    :: Text
  , reqPathParts :: [Dynamic t (Either Text Text)]
  , qParams      :: [(Text, QueryPart t)]
  , reqBody      :: Maybe (Dynamic t (Either Text (o, Text)))
  , headers      :: [(Text, Dynamic t (Either Text Text))]
  , respHeaders  :: XhrResponseHeaders
  , authData     :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

data SomeXhrRequest = forall o. (IsXhrPayload o, Show o) => SomeXhrRequest (XhrRequest (Maybe o))

defReq :: Req t
defReq = Req "GET" [] [] (Nothing :: Maybe (Dynamic t (Either Text ((), Text)))) [] def Nothing

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
      -> Dynamic t (Either Text SomeXhrRequest)
reqToReflexRequest reqMeth reqHost req@(Req _ _ _ (reqBody :: Maybe (Dynamic t (Either Text (o, Text)))) _ _ _) =
  let t :: Dynamic t [Either Text Text]
      t = sequence $ reverse $ reqPathParts req

      baseUrl :: Dynamic t (Either Text Text)
      baseUrl = Right . showBaseUrl <$> reqHost

      urlParts :: Dynamic t (Either Text [Text])
      urlParts = fmap sequence t

      urlPath :: Dynamic t (Either Text Text)
      urlPath = (fmap.fmap)
                (T.intercalate "/" . fmap escape)
                urlParts

      queryPartString :: (Text, QueryPart t) -> Dynamic t (Maybe (Either Text Text))
      queryPartString (pName, qp) = case qp of
        QueryPartParam p -> ffor p $ \case
          Left e         -> Just (Left e)
          Right (Just a) -> Just (Right $ pName <> "=" <> escape a)
          Right Nothing  -> Nothing
        QueryPartParams ps -> ffor ps $ \pStrings ->
          if null pStrings
          then Nothing
          else Just . Right
               . T.intercalate "&"
               $ fmap (\p -> pName <> "=" <> escape p) pStrings
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
                   -> (Either Text (o, Text))
                   -> Either Text (XhrRequestConfig (Maybe o))
      mkConfigBody ehs rb = case (ehs, rb) of
                  (_, Left e)                     -> Left e
                  (Left e, _)                     -> Left e
                  (Right hs, Right (bBytes, bCT)) ->
                    Right $ XhrRequestConfig
                      { _xhrRequestConfig_sendData = Just bBytes
                      , _xhrRequestConfig_headers  =
                        Map.insert "Content-Type" bCT (Map.fromList hs)
                      , _xhrRequestConfig_user = Nothing
                      , _xhrRequestConfig_password = Nothing
                      , _xhrRequestConfig_responseType = Nothing
                      , _xhrRequestConfig_withCredentials = False
                      , _xhrRequestConfig_responseHeaders = def
                      }

      xhrOpts :: Dynamic t (Either Text (XhrRequestConfig (Maybe o)))
      xhrOpts = case reqBody of
        Nothing    -> ffor xhrHeaders $ \case
                               Left e -> Left e
                               Right hs -> Right $ def { _xhrRequestConfig_headers = Map.fromList hs
                                                       , _xhrRequestConfig_user = Nothing
                                                       , _xhrRequestConfig_password = Nothing
                                                       , _xhrRequestConfig_responseType = Nothing
                                                       , _xhrRequestConfig_withCredentials = False
                                                       , _xhrRequestConfig_sendData = Nothing
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
        Nothing   -> xhr
        Just auth -> liftA2 mkAuth auth xhr

      xhrReq = (liftA2 . liftA2) (\p opt -> SomeXhrRequest $ XhrRequest reqMeth p opt) xhrUrl (addAuth xhrOpts)

  in xhrReq

-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

instance IsXhrPayload o => IsXhrPayload (Maybe o) where
  sendXhrPayload xhr = maybe (sendXhrPayload xhr ()) (sendXhrPayload xhr)

instance (IsXhrPayload o, IsXhrPayload o') => IsXhrPayload (Either o o') where
  sendXhrPayload xhr = either (sendXhrPayload xhr) (sendXhrPayload xhr)

-- | This function performs the request
performRequests :: forall t m f tag. (SupportsServantReflex t m, Traversable f)
                => Text
                -> Dynamic t (f (Req t))
                -> Dynamic t BaseUrl
                -> ClientOptions
                -> Event t tag
                -> m (Event t (tag, f (Either Text XhrResponse)))
performRequests reqMeth rs reqHost opts trigger = do
  let xhrReqs =
          join $ (\(fxhr :: f (Req t)) -> sequence $
                     reqToReflexRequest reqMeth reqHost <$> fxhr) <$> rs

      -- xhrReqs = fmap snd <$> xhrReqsAndDebugs
      reqs :: Event t (Compose ((,) tag) f (Either Text SomeXhrRequest))
      reqs    = attachPromptlyDynWith
                (\fxhr t -> Compose (t, fxhr)) xhrReqs trigger

  resps <- performSomeRequestsAsync opts reqs
  return $ getCompose <$> resps

-- | Issues a collection of requests when the supplied Event fires.
-- When ALL requests from a given firing complete, the results are
-- collected and returned via the return Event.
performSomeRequestsAsync
    :: (MonadIO (Performable m),
        MonadJSM (Performable m),
        HasWebView (Performable m),
        PerformEvent t m,
        TriggerEvent t m,
        Traversable f
       )
    => ClientOptions
    -> Event t (f (Either Text SomeXhrRequest))
    -> m (Event t (f (Either Text XhrResponse)))
performSomeRequestsAsync opts sxhr =
    performSomeRequestsAsync' opts newXMLHttpRequest $ return <$> sxhr


------------------------------------------------------------------------------
-- | A modified version or Reflex.Dom.Xhr.performRequestsAsync
-- that accepts 'f (Either e (XhrRequestb))' events
performSomeRequestsAsync'
    :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f)
    => ClientOptions
    -> (forall b. IsXhrPayload b => XhrRequest b -> (a -> JSM ()) -> Performable m XMLHttpRequest)
    -> Event t (Performable m (f (Either Text SomeXhrRequest)))
    -> m (Event t (f (Either Text a)))
performSomeRequestsAsync' opts newXhr req = performEventAsync $ ffor req $ \hrs cb -> do
  rs <- hrs
  resps <- forM rs $ \r -> case r of
      Left e -> do
          resp <- liftIO $ newMVar (Left e)
          return resp
      Right (SomeXhrRequest r') -> do
          resp <- liftIO newEmptyMVar
          r'' <- liftJSM $ (optsRequestFixup opts) r'
          _ <- newXhr r'' $ liftIO . putMVar resp . Right
          return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()


performRequestsCT
    :: (SupportsServantReflex t m,
        MimeUnrender ct a, Traversable f)
    => Proxy ct
    -> Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Event t tag
    -> m (Event t (f (ReqResult tag a)))
performRequestsCT ct reqMeth reqs reqHost opts trigger = do
  resps <- performRequests reqMeth reqs reqHost opts trigger
  let decodeResp x = first T.pack
                   . mimeUnrender ct
                   . JS.textToJSString
                   =<< note "No body text" (_xhrResponse_responseText x)
  return $ fmap
      (\(t,rs) -> ffor rs $ \r -> case r of
              Left e  -> RequestFailure t e
              Right g -> evalResponse decodeResp (t,g)
      )
      resps


performRequestsNoBody
    :: (SupportsServantReflex t m,
        Traversable f)
    => Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Event t tag
    -> m (Event t (f (ReqResult tag NoContent)))
performRequestsNoBody reqMeth reqs reqHost opts trigger = do
  resps <- performRequests reqMeth reqs reqHost opts trigger
  let decodeResp = const $ Right NoContent
  return $ ffor resps $ \(tag,rs) -> ffor rs $ \r -> case r of
      Left e  -> RequestFailure tag e
      Right g -> evalResponse decodeResp (tag,g)


------------------------------------------------------------------------------
evalResponse
    :: (XhrResponse -> Either Text a)
    -> (tag, XhrResponse)
    -> ReqResult tag a
evalResponse decodeRes (tag, xhr) =
    let status = _xhrResponse_status xhr
        okStatus = status >= 200 && status < 400
        errMsg = fromMaybe
            ("Empty response with error code " <>
                T.pack (show $ _xhrResponse_status xhr))
            (_xhrResponse_responseText xhr)
        respPayld  = if okStatus
                     then either
                          (\e -> ResponseFailure tag e xhr)
                          (\v -> ResponseSuccess tag v xhr)
                          (decodeRes xhr)
                     else ResponseFailure tag errMsg xhr
    in respPayld

------------------------------------------------------------------------------
-- | Utility for simultaneously accessing/filtering Success and Failure
-- response 'Event's,
fanReqResult :: Reflex t => Event t (ReqResult tag a) -> (Event t Text, Event t a)
fanReqResult = fmapMaybe reqFailure &&& fmapMaybe reqSuccess

------------------------------------------------------------------------------
-- | Utility for simultaneously accessing/filtering Success and Failure
-- response 'Event's, but keeping the request tag
fanReqResult' :: Reflex t => Event t (ReqResult tag a) -> (Event t (tag, Text), Event t (tag, a))
fanReqResult' = fmapMaybe reqFailure' &&& fmapMaybe reqSuccess'


-- | Similar to 'Servant.API.ContentType.MimeUnrender' but with the differnce that
-- the second argument expects a JSString instead of ByteString.
-- This can lead to better performance ghcjs.
class Accept contentType => MimeUnrender contentType a where
  mimeUnrender :: Proxy contentType -> JS.JSString -> Either String a

instance FromJSON a => MimeUnrender JSON a where
  mimeUnrender _ = note "Failed to decode response" . jsonDecode

-- | @urlDecodeAsForm@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance FromForm a => MimeUnrender FormUrlEncoded a where
    mimeUnrender _ = first T.unpack . urlDecodeAsForm . Builder.toLazyByteString . TE.encodeUtf8Builder . JS.textFromJSString

-- | @left show . TextL.decodeUtf8'@
instance MimeUnrender PlainText TL.Text where
    mimeUnrender _ = pure . JS.lazyTextFromJSString

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender PlainText T.Text where
    mimeUnrender _ = pure . JS.textFromJSString

-- | @Right . BC.unpack@
instance MimeUnrender PlainText String where
    mimeUnrender _ = Right . T.unpack . JS.textFromJSString

-- | @Right . id@
instance MimeUnrender OctetStream ByteString where
    mimeUnrender _ = pure . Builder.toLazyByteString . TE.encodeUtf8Builder . JS.textFromJSString

-- | @Right . toStrict@
instance MimeUnrender OctetStream BS.ByteString where
    mimeUnrender _ = pure . TE.encodeUtf8 . JS.textFromJSString

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

fmapL :: (e -> e') -> Either e a -> Either e' a
fmapL _ (Right a) = Right a
fmapL f (Left e)  = Left (f e)

builderToText :: Builder.Builder -> T.Text
builderToText = TE.decodeUtf8 . BL.toStrict . Builder.toLazyByteString

escape :: T.Text -> T.Text
escape = T.pack . N.escapeURIString (not . N.isReserved) . T.unpack . TE.decodeUtf8 . BL.toStrict . Builder.toLazyByteString . toEncodedUrlPiece
