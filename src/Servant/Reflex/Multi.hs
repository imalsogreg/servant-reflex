{-# LANGUAGE AllowAmbiguousTypes  #-}
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

module Servant.Reflex.Multi
    ( clientA
    , HasClientMulti(..)
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as E
import           Data.CaseInsensitive    (mk)
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

import           Reflex.Dom              (Dynamic, Event, Reflex,
                                          XhrRequest(..),
                                          XhrResponseHeaders(..),
                                          XhrResponse(..), ffor, fmapMaybe,
                                          leftmost, performRequestAsync,
                                          tagPromptlyDyn )
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)
import           Servant.Common.Req      (Req, ReqResult(..), QParam(..),
                                          QueryPart(..), addHeader, authData,
                                          defReq, prependToPathParts,
                                          performRequestCT,
                                          performRequestsCT,
                                          performRequestNoBody,
                                          performRequestsNoBody,
                                          performSomeRequestsAsync,
                                          qParamToQueryPart, reqBody,
                                          reqSuccess, reqFailure,
                                          reqMethod, respHeaders, response,
                                          qParams)
import          Servant.Reflex            (BuildHeaderKeysTo(..), toHeaders)

clientA :: (HasClientMulti t m layout f, Applicative f)
       => Proxy layout -> Proxy m -> Proxy f -> Dynamic t BaseUrl -> ClientMulti t m layout f
clientA p q f baseurl  = clientWithRouteMulti p q f (pure defReq) baseurl

class HasClientMulti t m layout f where
  type ClientMulti t m layout f :: *
  clientWithRouteMulti :: Proxy layout -> Proxy m -> Proxy f -> f (Req t) -> Dynamic t BaseUrl -> ClientMulti t m layout f

instance (HasClientMulti t m a f, HasClientMulti t m b f) => HasClientMulti t m (a :<|> b) f where
  type ClientMulti t m (a :<|> b) f = ClientMulti t m a f :<|> ClientMulti t m b f
  clientWithRouteMulti Proxy q f reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy a) q f reqs baseurl :<|>
    clientWithRouteMulti (Proxy :: Proxy b) q f reqs baseurl


instance (SupportsServantReflex t m, ToHttpApiData a, HasClientMulti t m sublayout f, Applicative f)
      => HasClientMulti t m (Capture capture a :> sublayout) f where

  type ClientMulti t m (Capture capture a :> sublayout) f =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f

  clientWithRouteMulti l q f reqs baseurl vals = clientWithRouteMulti (Proxy :: Proxy sublayout) q f reqs' baseurl
    where
      reqs' = prependToPathParts <$> ps <*> reqs
      ps    = (fmap .  fmap . fmap) toUrlPiece vals

-- VERB (Returning content) --
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts),
   SupportsServantReflex t m,
   Applicative f,
   Traversable f
  ) => HasClientMulti t m (Verb method status cts' a) f where

  type ClientMulti t m (Verb method status cts' a) f =
    Event t () -> m (Event t (f (ReqResult a)))

  clientWithRouteMulti _ _ f reqs baseurl =
    performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
            reqs' = (\r -> r { reqMethod = method }) <$> reqs

-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method, SupportsServantReflex t m, Traversable f) =>
  HasClientMulti t m (Verb method status cts NoContent) f where
  type ClientMulti t m (Verb method status cts NoContent) f =
    Event t () -> m (Event t (f (ReqResult NoContent)))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRouteMulti Proxy _ _ req baseurl =
    performRequestsNoBody method req baseurl
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)

instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
    ReflectMethod method, cts' ~ (ct ': cts),
    SupportsServantReflex t m,
    Traversable f
  ) => HasClientMulti t m (Verb method status cts' (Headers ls a)) f where
  type ClientMulti t m (Verb method status cts' (Headers ls a)) f =
    Event t () -> m (Event t (f (ReqResult (Headers ls a))))
  clientWithRouteMulti Proxy _ _ reqs baseurl = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl
    return $ (fmap . fmap) toHeaders <$> resp
    where reqs' = ffor reqs $ \r -> r { respHeaders =
                                       OnlyHeaders (Set.fromList (buildHeaderKeysTo (Proxy :: Proxy ls)))
                                      }


instance {-# OVERLAPPABLE #-}
  ( BuildHeadersTo ls,
    BuildHeaderKeysTo ls,
    ReflectMethod method,
    SupportsServantReflex t m,
    Traversable f
  ) => HasClientMulti t m (Verb method status cts (Headers ls NoContent)) f where
  type ClientMulti t m (Verb method status cts (Headers ls NoContent)) f
    = Event t () -> m (Event t (f (ReqResult (Headers ls NoContent))))
  clientWithRouteMulti Proxy _ _ reqs baseurl = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsNoBody method reqs' baseurl
    return $ (fmap . fmap) toHeaders <$> resp
    where reqs' = ffor reqs $ \req ->
                    req {respHeaders = OnlyHeaders (Set.fromList
                         (buildHeaderKeysTo (Proxy :: Proxy ls)))
                        }


instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f,
          SupportsServantReflex t m,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (Header sym a :> sublayout) f where

  type ClientMulti t m (Header sym a :> sublayout) f =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f

  clientWithRouteMulti Proxy f q reqs baseurl eVals =
    clientWithRouteMulti (Proxy :: Proxy sublayout) f
                    q
                    reqs'
                    baseurl
    where hname = T.pack $ symbolVal (Proxy :: Proxy sym)
          reqs' = (\eVal req -> Servant.Common.Req.addHeader hname eVal req)
                  <$> eVals <*> reqs


instance HasClientMulti t m sublayout f
  => HasClientMulti t m (HttpVersion :> sublayout) f where

  type ClientMulti t m (HttpVersion :> sublayout) f =
    ClientMulti t m sublayout f

  clientWithRouteMulti Proxy q f =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f


instance (KnownSymbol sym, ToHttpApiData a, HasClientMulti t m sublayout f, Reflex t, Traversable f, Applicative f)
      => HasClientMulti t m (QueryParam sym a :> sublayout) f where

  type ClientMulti t m (QueryParam sym a :> sublayout) f =
    f (Dynamic t (QParam a)) -> ClientMulti t m sublayout f

  -- if mparam = Nothing, we don't add it to the query string
  -- TODO: Check the above comment
  clientWithRouteMulti Proxy q f reqs baseurl mparams =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f
      reqs' baseurl

    where pname = symbolVal (Proxy :: Proxy sym)
          p prm = QueryPartParam $ fmap qParamToQueryPart prm
          paramPair mp = (T.pack pname, p mp)
          reqs' = (\param req -> req {qParams = paramPair param : qParams req})
                  <$> mparams <*> reqs


instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (QueryParams sym a :> sublayout) f where

  type ClientMulti t m (QueryParams sym a :> sublayout) f =
    f (Dynamic t [a]) -> ClientMulti t m sublayout f

  clientWithRouteMulti Proxy q f reqs baseurl paramlists =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f reqs' baseurl

      where req' l r = r { qParams =  (T.pack pname, params' l) : qParams r }
            pname   = symbolVal (Proxy :: Proxy sym)
            params' l = QueryPartParams $ (fmap . fmap) (toQueryParam)
                        l
            reqs' = req' <$> paramlists <*> reqs


instance (KnownSymbol sym,
          HasClientMulti t m sublayout f,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (QueryFlag sym :> sublayout) f where

  type ClientMulti t m (QueryFlag sym :> sublayout) f =
    f (Dynamic t Bool) -> ClientMulti t m sublayout f

  clientWithRouteMulti Proxy q f reqs baseurl flags =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f reqs' baseurl

    where req' f req = req { qParams = thisPair f : qParams req }
          thisPair f = (T.pack pName, QueryPartFlag f) :: (Text, QueryPart t)
          pName      = symbolVal (Proxy :: Proxy sym)
          reqs'      = req' <$> flags <*> reqs


instance (SupportsServantReflex t m,
          Traversable f) => HasClientMulti t m Raw f where
  type ClientMulti t m Raw f = f (Dynamic t (Either Text (XhrRequest ())))
                      -> Event t ()
                      -> m (Event t (f (ReqResult ())))

  clientWithRouteMulti Proxy Proxy Proxy oldReqs baseurl (rawReqs) triggers = do
    resps <- performSomeRequestsAsync (tagPromptlyDyn (sequence rawReqs) triggers )
    return $ (fmap . fmap) aux resps
    where
      aux (Right r) = ResponseSuccess () r
      aux (Left  e) = RequestFailure e


instance (MimeRender ct a,
          HasClientMulti t m sublayout f,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f where

  type ClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f

  clientWithRouteMulti Proxy q f reqs baseurl bodies =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f reqs' baseurl
       where req'        b r = r { reqBody = bodyBytesCT b }
             ctProxy         = Proxy :: Proxy ct
             ctString        = T.pack $ show $ contentType ctProxy
             bodyBytesCT b   = Just $ (fmap . fmap)
                               (\b' -> (mimeRender ctProxy b', ctString))
                               b
             reqs'           = req' <$> bodies <*> reqs


instance (KnownSymbol path,
          HasClientMulti t m sublayout f,
          Reflex t,
          Functor f) => HasClientMulti t m (path :> sublayout) f where
  type ClientMulti t m (path :> sublayout) f = ClientMulti t m sublayout f

  clientWithRouteMulti Proxy q f reqs baseurl =
     clientWithRouteMulti (Proxy :: Proxy sublayout) q f
                     (prependToPathParts (pure (Right $ T.pack p)) <$> reqs)
                     baseurl

    where p = symbolVal (Proxy :: Proxy path)


instance HasClientMulti t m api f => HasClientMulti t m (Vault :> api) f where
  type ClientMulti t m (Vault :> api) f = ClientMulti t m api f

  clientWithRouteMulti Proxy q f reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f reqs baseurl


instance HasClientMulti t m api f => HasClientMulti t m (RemoteHost :> api) f where
  type ClientMulti t m (RemoteHost :> api) f = ClientMulti t m api f

  clientWithRouteMulti Proxy q f reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f reqs baseurl


instance HasClientMulti t m api f => HasClientMulti t m (IsSecure :> api) f where
  type ClientMulti t m (IsSecure :> api) f = ClientMulti t m api f

  clientWithRouteMulti Proxy q f reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f reqs baseurl


instance (HasClientMulti t m api f, Reflex t, Traversable f, Applicative f)
      => HasClientMulti t m (BasicAuth realm usr :> api) f where

  type ClientMulti t m (BasicAuth realm usr :> api) f = f (Dynamic t (Maybe BasicAuthData))
                                               -> ClientMulti t m api f

  clientWithRouteMulti Proxy q f reqs baseurl authdatas =
    clientWithRouteMulti (Proxy :: Proxy api) q f reqs' baseurl
      where
        req'  a r = r { authData = Just a }
        reqs' = req' <$> authdatas <*> reqs
