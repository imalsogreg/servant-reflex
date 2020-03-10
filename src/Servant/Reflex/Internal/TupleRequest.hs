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
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Reflex.Internal.TupleRequest where

import           Control.Arrow                        (first, second)
import           Data.CaseInsensitive                 (mk)
import Control.Lens (over,set)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Default           as Default
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Set as Set
import           Data.Generics.Product
import Data.Proxy (Proxy(..))
import           GHC.TypeLits                         (KnownSymbol, symbolVal)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           GHC.Generics           (Generic)
import           Reflex.Dom.Xhr         (XhrRequest (..), XhrRequestConfig (..),
                                         XhrResponseHeaders (..),
                                         XhrResponseBody(..),
                                         XhrResponseType (..), XhrResponse(..))
import Servant.API hiding (HList(..))
import           Servant.API            ((:<|>), BasicAuthData, Raw,
                                         basicAuthPassword, basicAuthUsername)

import           Servant.Reflex                       (BuildHeaderKeysTo (..),
                                                       toHeaders)
import           Servant.Common.BaseUrl (SupportsServantReflex, BaseUrl (..), showBaseUrl)
import           Servant.Common.Req     (escape)



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
class EndpointR t m api f where

  type EInputs        t m api f :: [*]
  type EOutput        t m api f :: *

  mkReq :: HList (EInputs t m api f) -> ReqR
  getResp :: XhrResponse -> EOutput t m api f

data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)


class ToTuple xs where
  type Tuple xs :: *
  toTuple :: xs -> Tuple xs
  fromTuple :: Tuple xs -> xs

data Sealed a
data SealedRaw

type family Seal a where
  Seal (a :<|> b) = Seal a :<|> Seal b
  Seal Raw = SealedRaw
  Seal a = Sealed a

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


data ReqR = ReqR
  { reqRMethod      :: T.Text
  , reqRPathParts   :: [T.Text]
  , reqRParams      :: [(T.Text, QueryPart)]
  , reqRBody        :: Maybe (BL.ByteString, T.Text)
  , reqRHeaders     :: [(T.Text, T.Text)]
  , reqRRespHeaders :: XhrResponseHeaders
  , reqRAuthData    :: Maybe BasicAuthData
  } deriving (Generic)

instance Show ReqR where
  show (ReqR m pp ps b hs hs' a) = concat $ L.intersperse "\n" [
    "ReqR {"
    , ("reqRMethod = " <> show m)
    , ("reqRPathParts = " <> show pp)
    , ("reqRParams = " <> show ps)
    , ("reqRBody = " <> show b)
    , ("reqRHeaders = " <> show hs)
    , ("reqRRespHeaders = " <> show hs')
    , ("reqRAuthData = " <> maybe "Nothing" (const "Just <<redacted>>") a)
    ]

emptyReqR :: T.Text -> ReqR
emptyReqR method = ReqR method [] [] Nothing  [] (OnlyHeaders mempty) Nothing

data QueryPart = QueryPartParam  (Maybe Text)
               | QueryPartParams [Text]
               | QueryPartFlag   Bool
               deriving (Show)

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

      (headers, body) =
        let headers0 = M.fromList $ reqRHeaders req
        in case reqRBody req of
             Nothing      -> (headers0, "")
             Just (rb,ct) -> (M.insert "Content-Type" ct headers0,
                              E.decodeUtf8 $ BL.toStrict rb)

      xhrReqConfig = Default.def
        { _xhrRequestConfig_headers = headers
        , _xhrRequestConfig_user = E.decodeUtf8 . basicAuthUsername <$> reqRAuthData req
        , _xhrRequestConfig_password = E.decodeUtf8 . basicAuthPassword <$> reqRAuthData req
        , _xhrRequestConfig_responseType = Just XhrResponseType_ArrayBuffer
        , _xhrRequestConfig_sendData = body
        , _xhrRequestConfig_responseHeaders = reqRRespHeaders req
        , _xhrRequestConfig_withCredentials = False
        }
      xhrReq = XhrRequest (reqRMethod req) url xhrReqConfig
  in xhrReq


instance {-# OVERLAPPING #-}
  ( MimeUnrender ct a
  , ReflectMethod method
  , cts' ~ (ct ': cts)
  , SupportsServantReflex t m
  -- , Applicative f
  )
  => EndpointR t m (Verb method status cts' a) f where
  type EInputs t m (Verb method status cts' a) f = '[]
  type EOutput t m (Verb method status cts' a) f = Either String a

  mkReq _ = emptyReqR . E.decodeUtf8 $ reflectMethod (Proxy @method)
  getResp resp = case _xhrResponse_response resp of
    Just (XhrResponseBody_ArrayBuffer x) -> mimeUnrender (Proxy @ct) (BL.fromStrict x)

instance {-# OVERLAPPING #-}
  ( ReflectMethod method
  , SupportsServantReflex t m
  -- , Applicative f
  )
  => EndpointR t m (Verb method status cts' NoContent) f where
  type EInputs t m (Verb method status cts' NoContent) f = '[]
  type EOutput t m (Verb method status cts' NoContent) f = Either String NoContent

  mkReq _ = emptyReqR . E.decodeUtf8 $ reflectMethod (Proxy @method)
  getResp resp = Right NoContent -- TODO: Should EOutput be `NoContent` rather than Either... ?


instance {-# OVERLAPPING #-}
  ( MimeUnrender ct a
  , BuildHeadersTo hs
  , ReflectMethod method, cts' ~ (ct ': cts)
  , SupportsServantReflex t m
  -- , Applicative f
  , BuildHeaderKeysTo hs
  )
  => EndpointR t m (Verb method status cts' (Headers hs a)) f where
  type EInputs t m (Verb method status cts' (Headers hs a)) f = '[]
  type EOutput t m (Verb method status cts' (Headers hs a)) f = Either String (Headers hs a)

  mkReq _ = set (field @"reqRRespHeaders") (OnlyHeaders (Set.fromList (buildHeaderKeysTo (Proxy @hs)))) $ emptyReqR $ E.decodeUtf8 $ reflectMethod (Proxy @method)
  getResp resp = case _xhrResponse_response resp of
    Just (XhrResponseBody_ArrayBuffer x) ->
      let hs = buildHeadersTo
               . fmap (first (mk . E.encodeUtf8) . second E.encodeUtf8)
               . M.toList
               $ _xhrResponse_headers resp
      in fmap (flip Headers (hs)) $ mimeUnrender (Proxy @ct) (BL.fromStrict x)


instance {-# OVERLAPPING #-}
  (MimeUnrender ct a
  , BuildHeadersTo hs
  , BuildHeaderKeysTo hs
  , ReflectMethod method
  , SupportsServantReflex t m
  -- , Applicative f
  )
  => EndpointR t m (Verb method status cts' (Headers hs NoContent)) f where
  type EInputs t m (Verb method status cts' (Headers hs NoContent)) f = '[]
  type EOutput t m (Verb method status cts' (Headers hs NoContent)) f = Either String (Headers hs NoContent)

  mkReq _ = set (field @"reqRRespHeaders") (OnlyHeaders (Set.fromList (buildHeaderKeysTo (Proxy @hs)))) $ emptyReqR . E.decodeUtf8 $ reflectMethod (Proxy @method)
  getResp resp = case _xhrResponse_response resp of
    Just (XhrResponseBody_ArrayBuffer x) ->
      let hs = buildHeadersTo
               . fmap (first (mk . E.encodeUtf8) . second E.encodeUtf8)
               . M.toList
               $ _xhrResponse_headers resp
      in Right (Headers NoContent hs)


instance
  ( SupportsServantReflex t m
  , ToHttpApiData a
  , EndpointR t m sublayout f
  -- , Applicative f
  )
  => EndpointR t m (Capture capture a :> sublayout) f where

  type EInputs t m (Capture capture a :> sublayout) f = a ': EInputs t m sublayout f
  type EOutput t m (Capture capture a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) = (\a -> over (field @"reqRPathParts") (toUrlPiece a:)) x $
                       mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f

instance
  ( SupportsServantReflex t m
  , ToHttpApiData a
  , EndpointR t m sublayout f
  , Applicative f
  )
  => EndpointR t m (CaptureAll capture a :> sublayout) f where

  type EInputs t m (CaptureAll capture a :> sublayout) f = [a] ': EInputs t m sublayout f
  type EOutput t m (CaptureAll capture a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) = (\a -> over (field @"reqRPathParts") (fmap toUrlPiece a <>)) x $
                       mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f


instance
  (SupportsServantReflex t m
  , KnownSymbol sym
  , EndpointR t m sublayout f
  -- , Applicative f
  )
  => EndpointR t m (sym :> sublayout) f where

  type EInputs t m (sym :> sublayout) f = EInputs t m sublayout f
  type EOutput t m (sym :> sublayout) f = EOutput t m sublayout f

  mkReq r = over (field @"reqRPathParts") (T.pack (symbolVal (Proxy @sym)) :) $ mkReq @t @m @sublayout @f r

  getResp = getResp @t @m @sublayout @f

instance
  ( SupportsServantReflex t m
  , ToHttpApiData a
  , EndpointR t m sublayout f
  , KnownSymbol sym
  -- , Applicative f
  )
  => EndpointR t m (QueryParam sym a :> sublayout) f where

  type EInputs t m (QueryParam sym a :> sublayout) f = Maybe a ': EInputs t m sublayout f
  type EOutput t m (QueryParam sym a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) = over (field @"reqRParams") ((T.pack $ symbolVal (Proxy @sym),
                                                   QueryPartParam $ fmap (toQueryParam) x) : ) $
    mkReq @t @m @sublayout @f xs


  getResp = getResp @t @m @sublayout @f

instance
  ( SupportsServantReflex t m
  , ToHttpApiData a
  , EndpointR t m sublayout f
  , KnownSymbol sym
  -- , Applicative f
  )
  => EndpointR t m (QueryParams sym a :> sublayout) f where

  type EInputs t m (QueryParams sym a :> sublayout) f = [a] ': EInputs t m sublayout f
  type EOutput t m (QueryParams sym a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) =
    let newParams = (T.pack (symbolVal (Proxy @sym)), QueryPartParams (toQueryParam <$> x))
    in over (field @"reqRParams") (newParams :) $ mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f

instance
  ( SupportsServantReflex t m
  , EndpointR t m sublayout f
  , KnownSymbol sym
  -- , Applicative f
  )
  => EndpointR t m (QueryFlag sym :> sublayout) f where

  type EInputs t m (QueryFlag sym :> sublayout) f = Bool ': EInputs t m sublayout f
  type EOutput t m (QueryFlag sym :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) =
    let newParam = (T.pack (symbolVal (Proxy @sym)), QueryPartFlag x)
    in over (field @"reqRParams") (newParam :) $ mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f

instance
  ( SupportsServantReflex t m
  , EndpointR t m sublayout f
  , MimeRender ct a
  -- , Applicative f
  )
  => EndpointR t m (ReqBody (ct ': cts) a :> sublayout) f where


  type EInputs t m (ReqBody (ct ': cts) a :> sublayout) f = a ': EInputs t m sublayout f
  type EOutput t m (ReqBody (ct ': cts) a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) =
    set (field @"reqRBody") (Just (mimeRender (Proxy @ct) x, T.pack (show (contentType (Proxy @ct))))) $ mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f


instance
  ( SupportsServantReflex t m
  , EndpointR t m sublayout f
  , KnownSymbol sym
  , ToHttpApiData a
  -- , Applicative f
  )
  => EndpointR t m (Header sym a :> sublayout) f where


  type EInputs t m (Header sym a :> sublayout) f = a ': EInputs t m sublayout f
  type EOutput t m (Header sym a :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) =
    over (field @"reqRHeaders")
         -- ((symbolVal (Proxy @sym), (mimeRender (Proxy @a) x)) :)
         ((T.pack $ symbolVal (Proxy @sym), E.decodeUtf8 $ toHeader x) :)
    $ mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f

#if MIN_VERSION_servant(0,13,0)
-- TODO: Untested
instance (SupportsServantReflex t m,
          EndpointR t m sublayout f,
          KnownSymbol sym,
          ToHttpApiData a,
          Applicative f
         ) => EndpointR t m (Summary desc :> sublayout) f where

  type EInputs t m (Summary desc :> sublayout) f = EInputs t m sublayout f
  type EOutput t m (Summary desc :> sublayout) f = EOutput t m sublayout f

  mkReq   = mkReq @t @m @sublayout @f

  getResp = getResp @t @m @sublayout @f
#endif


#if MIN_VERSION_servant(0,13,0)
-- TODO: Untested
instance (SupportsServantReflex t m,
          EndpointR t m sublayout f,
          KnownSymbol sym,
          ToHttpApiData a,
          Applicative f
         ) => EndpointR t m (Description desc :> sublayout) f where

  type EInputs t m (Description desc :> sublayout) f = EInputs t m sublayout f
  type EOutput t m (Description desc :> sublayout) f = EOutput t m sublayout f

  mkReq   = mkReq @t @m @sublayout @f

  getResp = getResp @t @m @sublayout @f
#endif

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f,
          KnownSymbol sym,
          ToHttpApiData a,
          Applicative f
         ) => EndpointR t m (IsSecure :> sublayout) f where


  type EInputs t m (IsSecure :> sublayout) f = EInputs t m sublayout f
  type EOutput t m (IsSecure :> sublayout) f = EOutput t m sublayout f

  mkReq   = mkReq @t @m @sublayout @f

  getResp = getResp @t @m @sublayout @f

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f,
          KnownSymbol sym,
          ToHttpApiData a,
          Applicative f
         ) => EndpointR t m (Vault :> sublayout) f where


  type EInputs t m (Vault :> sublayout) f = EInputs t m sublayout f
  type EOutput t m (Vault :> sublayout) f = EOutput t m sublayout f

  mkReq   = mkReq @t @m @sublayout @f

  getResp = getResp @t @m @sublayout @f

instance (SupportsServantReflex t m,
          EndpointR t m sublayout f,
          Applicative f
         ) => EndpointR t m (BasicAuth realm usr :> sublayout) f where

  type EInputs t m (BasicAuth realm usr :> sublayout) f = BasicAuthData ': EInputs t m sublayout f
  type EOutput t m (BasicAuth realm usr :> sublayout) f = EOutput t m sublayout f

  mkReq (HCons x xs) =
    set (field @"reqRAuthData") (Just x) $ mkReq @t @m @sublayout @f xs

  getResp = getResp @t @m @sublayout @f
