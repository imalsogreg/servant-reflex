{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- #include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.EventAPI where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Lazy    as BL
import           Data.CaseInsensitive    (mk)
import           Data.Default            (Default(..), def)
import           Data.Proxy              (Proxy (..))
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as T
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
import           Servant.API.ResponseHeaders (HList(..))
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)
import           Servant.Common.Req      (Req, ReqResult(..), QParam(..),
                                          QueryPart(..), addHeader, authData,
                                          defReq, prependToPathParts,
                                          performRequestCT,
                                          performRequestNoBody,
                                          qParamToQueryPart, reqBody,
                                          reqSuccess, reqFailure,
                                          reqMethod, respHeaders, response,
                                          qParams)
import           Reflex.Dom              (Dynamic, Event, Reflex,
                                          XhrRequest(..),
                                          XhrResponseHeaders(..),
                                          XhrResponse(..), ffor, fmapMaybe,
                                          leftmost, performRequestAsync,
                                          tagPromptlyDyn )

data PointReq = PointReq
    { prMethod :: Text
    , prPathParts :: [Text]
    , prQueryParams :: [(Text, Text)]
    , prReqBody :: Maybe (BL.ByteString, Text)
    , prHeaders :: [(Text,Text)]
    , prRespHeaders :: XhrResponseHeaders
    , prAuthData :: Maybe BasicAuthData
    }

defReq :: PointReq
defReq = PointReq "GET" [] [] Nothing [] def Nothing

-- type PointClient to from = (to -> XhrRequest Text, XhrResponse -> Maybe from)

class HasPointClient layout where
    type ToXhr       layout :: *
    type FromXhr     layout :: *
    type PointClient layout to from :: *
    pointClientWithRoute
        :: Proxy layout
        -> PointReq
        -> BaseUrl
        -> PointClient layout (ToXhr layout) (FromXhr layout)
        -- -> PointClient layout
        -- -> (to -> XhrRequest Text, XhrResponse -> Maybe from)

instance (HasPointClient a, HasPointClient b) => HasPointClient (a :<|> b) where
    type (ToXhr (a :<|> b)) = ToXhr a :<|> ToXhr b
    type (FromXhr (a :<|> b)) = FromXhr a :<|> FromXhr b
    type (PointClient (a :<|> b) toBoth fromBoth) =
        PointClient a (toBoth, ToXhr a) (FromXhr a)
        :<|>
        PointClient b (toBoth, ToXhr b) (FromXhr b)
    pointClientWithRoute p req base =
        pointClientWithRoute (Proxy :: Proxy a) req base
        :<|>
        pointClientWithRoute (Proxy :: Proxy b) req base

-- instance (ToHttpApiData a, HasPointClient sublayout)
--     => HasPointClient (Capture capture a :> sublayout) where

--     type (ToXhr (Capture capture a :> sublayout)) = (a, ToXhr sublayout)
--     type (FromXhr (Capture capture a :> sublayout)) = FromXhr sublayout
--     type (PointClient (Capture capture a :> sublayout)) = sublayout

-- instance (HasPointClient (aTo,aFrom), HasPointClient (bTo,bFrom)) => HasPointClient ((aTo,aFrom) :<|> (bTo,bFrom)) where
--     type PointClient ((aTo,aFrom) :<|> (bTo,bFrom)) = PointClient (aTo,aFrom) :<|> PointClient (bTo,bFrom)
--     pointClientWithRoute _ req baseurl =
--         pointClientWithRoute (Proxy :: Proxy (aTo,aFrom)) req baseurl :<|>
--         pointClientWithRoute (Proxy :: Proxy (bTo,bFrom)) req baseurl

-- instance (ToHttpApiData a, HasPointClient (subTo,subFrom))
--       => HasPointClient (Capture capture a :> (subTo,subFrom)) where

--   type PointClient (Capture capture a :> (subTo,subFrom)) =
--     PointClient ((a,subTo), subFrom)

--   pointClientWithRoute Proxy req baseurl =
--     pointClientWithRoute (Proxy :: Proxy sublayout)
--                     (req {prPathParts = toUrlPiece val : prPathParts req})
--                     baseurl

