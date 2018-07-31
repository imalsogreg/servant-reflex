{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-} -- Temporary
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Reflex.RequestAPI where

import           Control.Lens
import           Data.Generics.Product
import           Data.Proxy
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import qualified Data.Text.Encoding as E
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics
import           GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex hiding (HList(..))
import Language.Javascript.JSaddle (MonadJSM)
import Servant.API hiding (HList(..))
import Servant.Common.Req hiding (QueryPart(..))
import Servant.Common.BaseUrl

import Reflex.Dom hiding (HList(..))

data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

clientR :: forall t m layout f tag. (HasClientR t m (Seal layout) f tag, Applicative f, Reflex t)
        => Proxy layout -> Proxy (m :: * -> *) -> Proxy t -> Proxy f -> Proxy tag
        -> Dynamic t BaseUrl -> ClientR t m (Seal layout) f tag
clientR _ mP tP fP tagP url =
  clientRWithRoute (Proxy :: Proxy (Seal layout)) mP fP tagP undefined url undefined

-- | A class for generating top-level the API client function
class HasClientR t (m :: * -> *) api (f :: * -> *) tag where
  type ClientR t m api f tag :: *
  clientRWithRoute :: Proxy api -> Proxy m -> Proxy f -> Proxy tag
                   -> Dynamic t (f (Req t)) -> Dynamic t BaseUrl
                   -> ClientOptions -> ClientR t m api f tag



-- | A class for generating endpoint-level API client functions
--   We need two separate type classes because the needs of endpoints
--   and top-level clients are incompatible
--
--   Endpoints need to accumulate a list of their input types from
--   the API definition (the @EInputs@ associated type) when building
--   up a function from a sequence of @:>@ combinators. But for
--   sequences of @:<|>@ combinators, such a listing doesn't make
--   sense. So, we use one class for endpoint-level recursion and
--   another for collecting endpoints into the top-level API
class EndpointR t m api f tag where

  type EndpointResult t m api f tag :: *
  type EInputs        t m api f tag :: [*]
  type EOutput        t m api f tag :: *

  mkReq :: HList (EInputs t m api f tag) -> ReqR
  getResp :: XhrResponse -> EOutput t m api f tag

  eInputs :: HList (EInputs t m api f tag)
  clientEndpoint :: Proxy api -> Proxy m -> Proxy f -> Proxy tag
                 -> f ReqR -> Dynamic t BaseUrl
                 -> ClientOptions -> EndpointResult t m api f tag


instance {-# OVERLAPPING #-} (HasClientR t m (layoutLeft) f tag,
                              HasClientR t m (layoutRight) f tag
                             ) => HasClientR t m (layoutLeft :<|> layoutRight) f tag where
  type ClientR t m (layoutLeft :<|> layoutRight) f tag =
    ClientR t m (layoutLeft) f tag :<|> ClientR t m (layoutRight) f tag
  clientRWithRoute _ tP mP tagP _ url opts =
    clientRWithRoute (Proxy @(layoutLeft)) tP mP tagP undefined url opts
    :<|>
    clientRWithRoute (Proxy @(layoutRight)) tP mP tagP undefined url opts



-- instance {-# OVERLAPPING #-} (HasClientR t m (Sealed layoutLeft) f tag,
--                               HasClientR t m (Sealed layoutRight) f tag
--                              ) => HasClientR t m (Sealed layoutLeft :<|> Sealed layoutRight) f tag where
--   type ClientR t m (Sealed layoutLeft :<|> Sealed layoutRight) f tag =
--     ClientR t m (Sealed layoutLeft) f tag :<|> ClientR t m (Sealed layoutRight) f tag
--   clientRWithRoute _ tP mP tagP _ url opts =
--     clientRWithRoute (Proxy @(Sealed layoutLeft)) tP mP tagP undefined url opts
--     :<|>
--     clientRWithRoute (Proxy @(Sealed layoutRight)) tP mP tagP undefined url opts


class ToTuple xs where
  type Tuple xs :: *
  toTuple :: xs -> Tuple xs
  fromTuple :: Tuple xs -> xs

-- class FromTuple xs where
--   type UnTuple xs :: *
--   fromTuple :: Tuple xs -> xs

data Sealed a

type family Seal a where
  Seal (a :<|> b) = Seal a :<|> Seal b
  Seal a = Sealed a

-- | Turn a single Endpoint into a client, by turning the input HList
--   into a tuple
instance {-# OVERLAPPING #-} forall t m layout f tag .(EndpointR t m layout f tag,
          ToTuple (HList (EInputs t m layout f tag)),
          Monad m
          , Traversable f
          , SupportsServantReflex t m
         ) => HasClientR t (m :: * -> *) (Sealed layout) (f :: * -> *) tag where
  type ClientR t m (Sealed layout) f tag =
    Event t (f (Tuple (HList (EInputs t m layout f tag))))
    -> m (Event t (f (EOutput t m layout f tag)))
  clientRWithRoute _ mP fP tagP dReq url opts (reqs :: Event t (f (Tuple (HList (EInputs t m layout f tag))))) = do
    let reqTuples = fmap (fromTuple @(HList (EInputs t m layout f tag))) <$> reqs
        reqs'      = ffor (attachPromptlyDyn url reqTuples) $ \(bUrl, inputs) ->
                       mkXhrRequest bUrl .  mkReq @t @m @layout @f @tag <$> inputs
    resps <- performRequestsAsync reqs'
    return (fmap (getResp @t @m @layout @f @tag) <$> resps)


instance (SupportsServantReflex t m, Traversable f) => HasClientR t m Raw f tag where
  type ClientR t m Raw f tag = Event t (f (XhrRequest T.Text)) -> m (Event t (f XhrResponse))
  clientRWithRoute _ mP fP tagP dReq url opts reqs = performRequestsAsync reqs


instance (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts), SupportsServantReflex t m, Applicative f
         ) => EndpointR t m (Verb method status cts' a) f (tag :: *) where
  type EndpointResult t m (Verb method status cts' a) f tag = Event t tag -> m (Event t (ReqResult tag a))
  type EInputs t m (Verb method status cts' a) f tag = '[]
  type EOutput t m (Verb method status cts' a) f tag = Either String a

  eInputs = HNil
  clientEndpoint _ _ _ _ _ _ _ = undefined
  mkReq _ = emptyReqR . E.decodeUtf8 $ reflectMethod (Proxy @method)
  getResp resp = case _xhrResponse_response resp of
    Just (XhrResponseBody_ArrayBuffer x) -> mimeUnrender (Proxy @ct) (BL.fromStrict x)

-- type ClientFunction t m f tag ein eout = Event t (f (tag, Tuple (HList ein))) -> m (Event t (f (tag, eout)))

instance (SupportsServantReflex t m,
          ToHttpApiData a,
          EndpointR t m sublayout f tag,
          Applicative f
         ) => EndpointR t m (Capture capture a :> sublayout) f (tag :: *) where

  type EndpointResult  t m (Capture capture a :> sublayout) f tag =
    f a ->
      EndpointResult t m sublayout f tag

  type EInputs t m (Capture capture a :> sublayout) f tag = a ': EInputs t m sublayout f tag
  type EOutput t m (Capture capture a :> sublayout) f tag = EOutput t m sublayout f tag
  eInputs = undefined
  clientEndpoint _ pM pF pTag reqs url opts vals =
    clientEndpoint (Proxy :: Proxy sublayout) pM pF pTag reqs' url opts
    where reqs' = (\v -> over (field @"reqRPathParts") (toUrlPiece v :))
                  <$> vals
                  <*> reqs
  mkReq (HCons x xs) = (\a -> over (field @"reqRPathParts") (toUrlPiece a:)) x $ mkReq @t @m @sublayout @f @tag xs

  getResp = getResp @t @m @sublayout @f @tag


instance (SupportsServantReflex t m,
          KnownSymbol sym,
          EndpointR t m sublayout f tag,
          Applicative f
         ) => EndpointR t m (sym :> sublayout) f (tag :: *) where

  type EndpointResult  t m (sym :> sublayout) f tag =
      EndpointResult t m sublayout f tag

  type EInputs t m (sym :> sublayout) f tag = EInputs t m sublayout f tag
  type EOutput t m (sym :> sublayout) f tag = EOutput t m sublayout f tag
  clientEndpoint _ pM pF pTag reqs url opts =
    clientEndpoint (Proxy :: Proxy sublayout) pM pF pTag reqs' url opts
    where reqs' = (over (field @"reqRPathParts") (T.pack (symbolVal (Proxy @sym)) :))
                  <$> reqs
  mkReq r = over (field @"reqRPathParts") (T.pack (symbolVal (Proxy @sym)) :) $ mkReq @t @m @sublayout @f @tag r

  getResp = getResp @t @m @sublayout @f @tag
instance (SupportsServantReflex t m,
          ToHttpApiData a,
          EndpointR t m sublayout f tag,
          KnownSymbol sym,
          Applicative f
         ) => EndpointR t m (QueryParam sym a :> sublayout) f (tag :: *) where

  type EndpointResult  t m (QueryParam sym a :> sublayout) f tag =
    f a ->
      EndpointResult t m sublayout f tag

  type EInputs t m (QueryParam sym a :> sublayout) f tag = Maybe a ': EInputs t m sublayout f tag
  type EOutput t m (QueryParam sym a :> sublayout) f tag = EOutput t m sublayout f tag
  clientEndpoint _ pM pF pTag reqs url opts vals = undefined -- TODO
    -- clientEndpoint (Proxy :: Proxy sublayout) pM pF pTag reqs' url opts
    -- where reqs' = (\v -> over (field @"reqRPathParts") (toUrlPiece v :))
    --               <$> vals
    --               <*> reqs
  mkReq (HCons x xs) = over (field @"reqRParams") ((T.pack $ symbolVal (Proxy @sym),
                                                   QueryPartParam $ fmap (toQueryParam) x) : ) $
    mkReq @t @m @sublayout @f @tag xs


  getResp = getResp @t @m @sublayout @f @tag

instance (SupportsServantReflex t m,
          ToHttpApiData a,
          EndpointR t m sublayout f tag,
          KnownSymbol sym,
          Applicative f
         ) => EndpointR t m (QueryParams sym a :> sublayout) f (tag :: *) where

  type EndpointResult  t m (QueryParams sym a :> sublayout) f tag =
    f a ->
      EndpointResult t m sublayout f tag

  type EInputs t m (QueryParams sym a :> sublayout) f tag = [a] ': EInputs t m sublayout f tag
  type EOutput t m (QueryParams sym a :> sublayout) f tag = EOutput t m sublayout f tag
  eInputs = undefined
  clientEndpoint _ pM pF pTag reqs url opts vals = undefined -- TODO
  mkReq (HCons x xs) =
    let newParams = (T.pack (symbolVal (Proxy @sym)), QueryPartParams (toQueryParam <$> x))
    in over (field @"reqRParams") (newParams :) $ mkReq @t @m @sublayout @f @tag xs

  getResp = getResp @t @m @sublayout @f @tag

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f tag,
          KnownSymbol sym,
          Applicative f
         ) => EndpointR t m (QueryFlag sym :> sublayout) f (tag :: *) where

  type EndpointResult  t m (QueryFlag sym :> sublayout) f tag =
    f Bool ->
      EndpointResult t m sublayout f tag

  type EInputs t m (QueryFlag sym :> sublayout) f tag = Bool ': EInputs t m sublayout f tag
  type EOutput t m (QueryFlag sym :> sublayout) f tag = EOutput t m sublayout f tag

  mkReq (HCons x xs) =
    let newParam = (T.pack (symbolVal (Proxy @sym)), QueryPartFlag x)
    in over (field @"reqRParams") (newParam :) $ mkReq @t @m @sublayout @f @tag xs

  getResp = getResp @t @m @sublayout @f @tag

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f tag,
          MimeRender ct a,
          Applicative f
         ) => EndpointR t m (ReqBody (ct ': cts) a :> sublayout) f (tag :: *) where

  type EndpointResult  t m ( ReqBody (ct ': cts) a :> sublayout) f tag =
    f Bool ->
      EndpointResult t m sublayout f tag

  type EInputs t m (ReqBody (ct ': cts) a :> sublayout) f tag = a ': EInputs t m sublayout f tag
  type EOutput t m (ReqBody (ct ': cts) a :> sublayout) f tag = EOutput t m sublayout f tag

  mkReq (HCons x xs) =
    set (field @"reqRBody") (Just (mimeRender (Proxy @ct) x, T.pack (show (contentType (Proxy @ct))))) $ mkReq @t @m @sublayout @f @tag xs

  getResp = getResp @t @m @sublayout @f @tag

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f tag,
          Applicative f
         ) => EndpointR t m (BasicAuth realm usr :> sublayout) f (tag :: *) where

  type EndpointResult  t m ( BasicAuth realm usr :> sublayout) f tag =
    f Bool ->
      EndpointResult t m sublayout f tag

  type EInputs t m (BasicAuth realm usr :> sublayout) f tag = BasicAuthData ': EInputs t m sublayout f tag
  type EOutput t m (BasicAuth realm usr :> sublayout) f tag = EOutput t m sublayout f tag

  mkReq (HCons x xs) =
    set (field @"reqRAuthData") (Just x) $ mkReq @t @m @sublayout @f @tag xs

  getResp = getResp @t @m @sublayout @f @tag


instance ToTuple (HList '[]) where
  type Tuple (HList '[]) = ()
  toTuple _ = ()
  fromTuple _ = HNil

instance ToTuple (HList '[a]) where
  type Tuple (HList '[a]) = a
  toTuple (HCons a HNil) = a
  fromTuple a = HCons a HNil

instance ToTuple (HList '[a,b]) where
  type Tuple (HList '[a,b]) = (a,b)
  toTuple (HCons a (HCons b HNil)) = (a,b)
  fromTuple (a,b) = HCons a (HCons b HNil)

instance ToTuple (HList '[a,b,c]) where
  type Tuple (HList '[a,b,c]) = (a,b,c)
  toTuple (HCons a (HCons b (HCons c HNil))) = (a,b,c)
  fromTuple (a,b,c) = HCons a (HCons b (HCons c HNil))

type MyAPI = Capture "hello" Int :> Capture "hello2" Bool :> Get '[JSON] Int
        :<|> Get '[JSON] Bool


m :: forall t m.MonadWidget t m => m ()
m = do
  let (a :<|> b) = clientR (Proxy @(MyAPI)) (Proxy :: Proxy m) (Proxy :: Proxy t)
                           (Proxy @[]) (Proxy @Integer) undefined
  btn <- button "Hello"
  resA <- a (fmap (:[]) ((5,True) <$ btn))
  resB <- b (fmap (:[]) (() <$ btn))
  display =<< holdDyn "waiting" (leftmost [show <$> resA, show <$> resB])
  return ()

data ReqR = ReqR
  { reqRMethod      :: T.Text
  , reqRPathParts   :: [T.Text]
  , reqRParams      :: [(T.Text, QueryPart)]
  , reqRBody        :: Maybe (BL.ByteString, T.Text)
  , reqRHeaders     :: [(T.Text, T.Text)]
  , reqRRespHeaders :: XhrResponseHeaders
  , reqRAuthData    :: Maybe BasicAuthData
  } deriving (Generic)

emptyReqR :: T.Text -> ReqR
emptyReqR method = ReqR method [] [] Nothing  [] (OnlyHeaders mempty) Nothing

data QueryPart = QueryPartParam  (Maybe Text)
               | QueryPartParams [Text]
               | QueryPartFlag   Bool

mkXhrRequest :: BaseUrl -> ReqR -> XhrRequest T.Text
mkXhrRequest reqHost req =
  let path = T.intercalate "/" $ reqRPathParts req

      queryPartString :: (Text, QueryPart) -> Maybe Text
      queryPartString (pName, qp) = case qp of

        QueryPartParam Nothing  -> Nothing
        QueryPartParam (Just a) -> Just (pName <> "=" <> escape a)

        QueryPartParams ps      -> Just . T.intercalate "&"
                                        $ fmap (\p -> pName <> "=" <> escape p)
                                        ps

        QueryPartFlag True     -> Just pName
        QueryPartFlag False    -> Nothing

      (</>) :: Text -> Text -> Text
      x </> y | ("/" `T.isSuffixOf` x) || ("/" `T.isPrefixOf` y) = x <> y
              | otherwise = x <> "/" <> y

      queryString = T.intercalate "&" . catMaybes . map queryPartString $ reqRParams req
      url  = showBaseUrl reqHost </> path <> if T.null queryString then "" else (T.cons '?' queryString)

      xhrHeaders = [] -- TODO

      xhrReqConfig = def
        { _xhrRequestConfig_headers = M.fromList xhrHeaders
        , _xhrRequestConfig_user = E.decodeUtf8 . basicAuthUsername <$> reqRAuthData req
        , _xhrRequestConfig_password = E.decodeUtf8 . basicAuthPassword <$> reqRAuthData req
        , _xhrRequestConfig_responseType = Just XhrResponseType_ArrayBuffer
        , _xhrRequestConfig_sendData = "" -- TODO
        , _xhrRequestConfig_withCredentials = False
        }
      xhrReq = XhrRequest (reqRMethod req) url xhrReqConfig
  in xhrReq
