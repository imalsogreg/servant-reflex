{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Servant.Reflex.Pure where


------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as E
import           Data.CaseInsensitive    (mk)
import           Data.Functor.Identity
import           Data.Proxy              (Proxy (..))
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as T
import           GHC.Exts                (Constraint)
import           GHC.TypeLits            (KnownSymbol, symbolVal)
import           Servant.API             ((:<|>)(..),(:>), BasicAuth,
                                          BasicAuthData, BuildHeadersTo(..),
                                          Capture, contentType, Header,
                                          Headers(..), HttpVersion, IsSecure,
                                          MimeRender(..), MimeUnrender,
                                          NoContent, QueryFlag, QueryParam,
                                          QueryParams, Raw, ReflectMethod(..),
                                          RemoteHost, ReqBody,
                                          ToHttpApiData(..), Vault, Verb)
import qualified Servant.Auth            as Auth
import qualified Servant.Client.Core     as S

import           Reflex.Dom
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)

data CallParts a = CallParts
  { _reqParts :: S.Request
  , _respDecoder :: S.Response -> Either Text a
  }


class HasClient (m :: * -> *) (api :: *) where
    type Client (m :: * -> *) (api :: *) :: *
    clientWithRoute :: Proxy m -> Proxy api -> CallParts r -> Client m api

instance (HasClient m a, HasClient m b) => HasClient m (a :<|> b) where
    type Client m (a :<|> b) = Client m a :<|> Client m b
    clientWithRoute pM pLayout cp = clientWithRoute pM (Proxy :: Proxy a) cp :<|> clientWithRoute pM (Proxy :: Proxy b) cp

instance (KnownSymbol capture, ToHttpApiData a, HasClient m rest) => HasClient m (Capture capture a :> rest) where
    type Client m (Capture capture a :> rest) = Either Text a -> Client m rest
    clientWithRoute pM pLayout a cp = clientWithRoute pM (Proxy :: Proxy rest) _
      where auxParts = _ -- cp { _reqParts = S.appendToPath _ (_reqParts cp) }
