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

module Servant.Reflex.TupleRequestMulti where

import           Control.Arrow               (first, second)
import           Control.Lens
import qualified Data.ByteString.Lazy        as BL
import           Data.CaseInsensitive        (mk)
import qualified Data.Default                as Default
import           Data.Generics.Product
import qualified Data.List                   as L
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes)
import           Data.Proxy
import           Data.Semigroup              ((<>))
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import           GHC.Generics
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import           Language.Javascript.JSaddle (MonadJSM)
import           Reflex                      hiding (HList (..))
import           Servant.API                 hiding (HList (..))
import           Servant.Common.BaseUrl
import           Servant.Common.Req          hiding (QueryPart (..))

import           Reflex.Dom.Xhr              hiding (HList (..))

import           Servant.Reflex              (BuildHeaderKeysTo (..), toHeaders)
import Servant.Reflex.Internal.TupleRequest

{-
TODO:
 - [x] :<|>
 - [x] Verb content
 - [x] Verb nocontent
 - [x] Verb (Headers content)
 - [x] Verb (Headers nocontent)
 - [ ] Stream
 - [X] Capture :> ...
 - [ ] Capture' :> ...
 - [x] CaptureAll :> ...
 - [x] QueryParam :> ...
 - [x] QueryParams :> ...
 - [x] QueryFlag :> ...
 - [x] ReqBody :> ...
 - [ ] Header' :> ...
 - [x] Header :> ...
 - [x] Summary :> ...
 - [x] Description :> ...
 - [x] path :> ...
 - [ ] RemoteHost :> ...
 - [x] IsSecure :> ...
 - [x] Vault :> ...
 - [ ] WithNamedContext :> ...
 - [ ] AuthenticatedRequest :> ...
 - [x] BasicAuth :> ...
 - [ ] EmptyClient :> ...
 - [ ] EmptyClient
 - [ ] HttpVersion :> ...
-}

clientR
  :: forall t m layout f
  . ( HasClientR t m (Seal layout) f
    -- , Applicative f
    , Reflex t
    )
  => Proxy layout
  -> Proxy (m :: * -> *)
  -> Proxy t
  -> Proxy f
  -> Dynamic t BaseUrl
  -> ClientR t m (Seal layout) f
clientR _ mP tP fP url =
  clientRWithRoute (Proxy :: Proxy (Seal layout)) mP fP undefined url undefined

-- | A class for generating top-level the API client function
class HasClientR t (m :: * -> *) api (f :: * -> *) where
  type ClientR t m api f :: *
  clientRWithRoute :: Proxy api -> Proxy m -> Proxy f
                   -> Dynamic t (f (Req t)) -> Dynamic t BaseUrl
                   -> ClientOptions -> ClientR t m api f



------------------------------------------------------------------------------
-- | :<|> APIs
instance {-# OVERLAPPING #-}
  (HasClientR t m (layoutLeft) f,
    HasClientR t m (layoutRight) f
  ) => HasClientR t m (layoutLeft :<|> layoutRight) f where
  type ClientR t m (layoutLeft :<|> layoutRight) f =
    ClientR t m layoutLeft f :<|> ClientR t m layoutRight f
  clientRWithRoute _ tP mP _ url opts =
    clientRWithRoute (Proxy @layoutLeft) tP mP undefined url opts
    :<|>
    clientRWithRoute (Proxy @layoutRight) tP mP undefined url opts

#if MIN_VERSION_servant (0,13,0)
-- TODO: Untested
instance HasClientR t m EmptyAPI f where
  type ClientR t m EmptyAPI f = EmptyAPI
  clientRWithRoute _ _ _ _ _ _ = EmptyAPI
#endif


-- | Turn a single Endpoint into a client, by turning the input HList
--   into a tuple
instance {-# OVERLAPPING #-} forall t m layout f .(EndpointR t m layout f,
          ToTuple (HList (EInputs t m layout f)),
          Monad m
          , Traversable f
          , SupportsServantReflex t m
         ) => HasClientR t (m :: * -> *) (Sealed layout) (f :: * -> *) where
  type ClientR t m (Sealed layout) f =
    Event t (f (Tuple (HList (EInputs t m layout f))))
    -> m (Event t (f (EOutput t m layout f)))
  clientRWithRoute _ mP fP dReq url opts (reqs :: Event t (f (Tuple (HList (EInputs t m layout f ))))) = do
    let reqTuples = fmap (fromTuple @(HList (EInputs t m layout f ))) <$> reqs

        -- TODO: replace these 2 lines w/ the next 2. The trace is for library debugging
        reqRs     = traceEventWith (concat . fmap ((<> "\n") . show)) $ fmap (mkReq @t @m @layout @f) <$> reqTuples
        reqs'     = ffor (attachPromptlyDyn url reqRs) $ \(bUrl, rs) -> mkXhrRequest bUrl <$> rs
        -- reqs'      = ffor (attachPromptlyDyn url reqTuples) $ \(bUrl, inputs) ->
        --                mkXhrRequest bUrl .  mkReq @t @m @layout @f <$> inputs
    resps <- performRequestsAsync $ reqs'
    return (fmap (getResp @t @m @layout @f) <$> resps)


instance (SupportsServantReflex t m, Traversable f) => HasClientR t m SealedRaw f where
  type ClientR t m SealedRaw f = Event t (f (XhrRequest T.Text)) -> m (Event t (f XhrResponse))
  clientRWithRoute _ mP fP dReq url opts reqs = performRequestsAsync reqs


