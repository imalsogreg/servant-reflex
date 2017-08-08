{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- #include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Reflex.Tuple
  ( client
  , BuildHeaderKeysTo(..)
  , toHeaders
  , HasClient(..)
  , module Servant.Common.Req
  , module Servant.Common.BaseUrl
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.CaseInsensitive   (mk)
import           Data.Functor.Identity
import qualified Data.Map               as Map
import           Data.Monoid            ((<>))
import           Data.Proxy             (Proxy (..))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           GHC.TypeLits           (KnownSymbol, symbolVal)
import           Servant.API            ((:<|>) (..), (:>), BasicAuth,
                                         BasicAuthData, BuildHeadersTo (..),
                                         Capture, Header, Headers (..),
                                         HttpVersion, IsSecure, MimeRender (..),
                                         MimeUnrender, NoContent, QueryFlag,
                                         QueryParam, QueryParams, Raw,
                                         ReflectMethod (..), RemoteHost,
                                         ReqBody, ToHttpApiData (..), Vault,
                                         Verb, contentType)

import           Reflex.Dom             (Dynamic, Event, Reflex,
                                         XhrRequest (..), XhrResponse (..),
                                         XhrResponseHeaders (..),
                                         attachPromptlyDynWith, constDyn, ffor,
                                         fmapMaybe, leftmost,
                                         performRequestsAsync)
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..),
                                         SupportsServantReflex, baseUrlWidget,
                                         showBaseUrl)
import           Servant.Common.Req     (QParam (..), QueryPart (..), Req,
                                         ReqIO, ReqResult (..), addHeaderIO,
                                         authData, defReqIO, evalResponse,
                                         performRequestsCT,
                                         performRequestsNoBody,
                                         performSomeRequestsAsync,
                                         prependToPathPartsIO, qParamToQueryPart,
                                         qParams, reqBody, reqFailure,
                                         reqMethodIO, reqSuccess, reqTag,
                                         respHeadersIO, response)

type family ToTuple a :: *

type instance ToTuple () = ()
type instance ToTuple (a,()) = a
type instance ToTuple (a,(b,())) = (a,b)
type instance ToTuple (a,(b,(c,()))) = (a,b,c)
type instance ToTuple (a,(b,(c,(d,())))) = (a,b,c)
type instance ToTuple (a,(b,(c,(d,(e,()))))) = (a,b,c,d,e)

client :: (HasClient layout t m, HasInp layout) =>  Proxy layout -> Proxy m -> BaseUrl -> Client layout t m
client = undefined

class HasInp layout where
    type Inp layout :: *
    type Out layout :: *
    inpToReq :: Proxy layout -> Inp layout -> ReqIO

class HasClient layout (t :: *) (m :: * -> *) where
    type Client layout t m :: *

instance (HasClient a t m,
          HasClient b t m,
          HasInp a,
          HasInp b
         ) => HasClient (a :<|> b) t m where
    type Client (a :<|> b) t m = Client a t m :<|> Client b t m

instance (ToHttpApiData a, HasInp sublayout) => HasInp (Capture capture a :> sublayout) where
    type Inp (Capture capture a :> sublayout) = (a, Inp sublayout)
    type Out (Capture capture a :> sublayout) = Out sublayout
    inpToReq _ (a, otherInp) = prependToPathPartsIO (toUrlPiece a) $ inpToReq (Proxy :: Proxy sublayout) otherInp

instance (HasInp (Capture capture a :> sublayout)) => HasClient (Capture capture a :> sublayout) t m where
    type Client (Capture capture a :> sublayout) t m =
        Event t (ToTuple (Inp (Capture capture a :> sublayout))) ->
        m (Event t (Out (Capture capture a :> sublayout)))



instance {-# OVERLAPPABLE #-}
    (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
    ) => HasInp (Verb method status cts' a) where
    type Inp (Verb method status cts' a) = ()
    type Out (Verb method status cts' a) = Either Text a
    inpToReq _ _ = defReqIO

instance {-# OVERLAPPABLE #-}
    (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
    ) => HasClient (Verb method status cts' a) t m where
    type Client (Verb method status cts' a) t m = Event t (ToTuple (Inp (Verb method status cts' a))) ->
         m (Event t (Out (Verb method status cts' a)))


instance {-# OVERLAPPING #-}
    (ReflectMethod method) => HasInp (Verb method status cts NoContent) where
    type Inp (Verb method status cts NoContent) = ()
    type Out (Verb method status cts NoContent) = Either Text NoContent


instance {-# OVERLAPPING #-}
    (ReflectMethod method, SupportsServantReflex t m)
    => HasClient (Verb method status cts NoContent) t m where
    type Client (Verb method status cts NoContent) t m =
        Event t (ToTuple (Inp (Verb method status cts NoContent))) ->
        m (Event t (Out (Verb method status cts NoContent)))


instance {-# OVERLAPPABLE #-}
    (MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
     ReflectMethod method, cts' ~ (ct ': cts)
    ) => HasInp (Verb method status cts' (Headers ls a)) where
    type Inp (Verb method status cts' (Headers ls a)) = ()
    type Out (Verb method status cts' (Headers ls a)) = Either Text (Headers ls a)
    inpToReq _ _ = defReqIO { respHeadersIO =
                              OnlyHeaders (Set.fromList
                                           (buildHeaderKeysTo
                                            (Proxy :: Proxy ls)))
                            }


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-}
    HasInp (Verb method status cts' (Headers ls a))
    => HasClient (Verb method status cts' (Headers ls a)) t m where
    type Client (Verb method status cts' (Headers ls a)) t m =
        Event t (ToTuple (Inp (Verb method status cts' (Headers ls a)))) ->
        m (Event t (Out (Verb method status cts' (Headers ls a))))


------------------------------------------------------------------------------
instance {-# OVERLAPPING #-}
    (BuildHeadersTo ls,
     BuildHeaderKeysTo ls
    ) => HasInp (Verb method status cts' (Headers ls NoContent)) where
    type Inp (Verb method status cts' (Headers ls NoContent)) = ()
    type Out (Verb method status cts' (Headers ls NoContent)) =
         Either Text (Headers ls NoContent)
    inpToReq _ _ = defReqIO { respHeadersIO =
                              OnlyHeaders (Set.fromList
                                           (buildHeaderKeysTo
                                               (Proxy :: Proxy ls)))
                            }


------------------------------------------------------------------------------
instance {-# OVERLAPPING #-}
    (BuildHeadersTo ls,
     BuildHeaderKeysTo ls,
     SupportsServantReflex t m
    ) => HasClient (Verb method status cts' (Headers ls NoContent)) t m where
    type Client (Verb method status cts' (Headers ls NoContent)) t m =
        Event t (ToTuple (Inp (Verb method status cts' (Headers ls NoContent)))) ->
        m (Event t (Out (Verb method status cts' (Headers ls NoContent))))


------------------------------------------------------------------------------
toHeaders
    :: BuildHeadersTo ls
    => ReqResult tag a
    -> ReqResult tag (Headers ls a)
toHeaders r =
  let toBS = E.encodeUtf8
      hdrs = maybe []
                   (\xhr -> fmap (\(h,v) -> (mk (toBS h), toBS v))
                     (Map.toList $ _xhrResponse_headers xhr))
                   (response r)
  in  ffor r $ \a -> Headers {getResponse = a
                             ,getHeadersHList = buildHeadersTo hdrs
                             }


------------------------------------------------------------------------------
class BuildHeaderKeysTo hs where
  buildHeaderKeysTo :: Proxy hs -> [T.Text]


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} BuildHeaderKeysTo '[]
  where buildHeaderKeysTo _ = []


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (BuildHeaderKeysTo xs, KnownSymbol h)
  => BuildHeaderKeysTo ((Header h v) ': xs) where
  buildHeaderKeysTo _ =
      T.pack (symbolVal (Proxy :: Proxy h)) :
      buildHeaderKeysTo (Proxy :: Proxy xs)


------------------------------------------------------------------------------
instance (KnownSymbol sym,
          ToHttpApiData a,
          HasInp sublayout
         ) => HasInp (Header sym a :> sublayout) where
    type Inp (Header sym a :> sublayout) = (Maybe a, Inp sublayout)
    type Out (Header sym a :> sublayout) = Out sublayout
    inpToReq _ (h, vs) =
        (addHeaderIO hname h) (inpToReq (Proxy :: Proxy sublayout) vs)
      where hname = T.pack $ symbolVal (Proxy :: Proxy sym)


------------------------------------------------------------------------------
instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClient sublayout t m
         ) => HasClient (Header sym a :> sublayout) t m where
    type Client (Header sym a :> sublayout) t m =
             Event t (ToTuple (Inp (Header sym a :> sublayout)))
             -> m (Event t (Out (Header sym a :> sublayout)))


------------------------------------------------------------------------------
