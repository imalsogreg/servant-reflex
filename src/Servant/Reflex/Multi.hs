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
import           Control.Arrow           (second)
import           Data.Functor.Compose
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
                                          leftmost, performRequestAsync, attachPromptlyDynWith,
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

clientA :: (HasClientMulti t m layout f tag, Applicative f)
       => Proxy layout -> Proxy m -> Proxy f -> Proxy tag -> Dynamic t BaseUrl -> ClientMulti t m layout f tag
clientA p q f tag baseurl  = clientWithRouteMulti p q f tag (pure defReq) baseurl

class HasClientMulti t m layout f (tag :: *) where
  type ClientMulti t m layout f tag :: *
  clientWithRouteMulti :: Proxy layout -> Proxy m -> Proxy f -> Proxy tag -> f (Req t) -> Dynamic t BaseUrl -> ClientMulti t m layout f tag

instance (HasClientMulti t m a f tag, HasClientMulti t m b f tag) => HasClientMulti t m (a :<|> b) f tag where
  type ClientMulti t m (a :<|> b) f tag = ClientMulti t m a f tag :<|> ClientMulti t m b f tag
  clientWithRouteMulti Proxy q f tag reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy a) q f tag reqs baseurl :<|>
    clientWithRouteMulti (Proxy :: Proxy b) q f tag reqs baseurl


instance (SupportsServantReflex t m, ToHttpApiData a, HasClientMulti t m sublayout f tag, Applicative f)
      => HasClientMulti t m (Capture capture a :> sublayout) f tag where

  type ClientMulti t m (Capture capture a :> sublayout) f tag =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteMulti l q f tag reqs baseurl vals = clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl
    where
      reqs' = prependToPathParts <$> ps <*> reqs
      ps    = (fmap .  fmap . fmap) toUrlPiece vals

-- VERB (Returning content) --
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  (MimeUnrender ct a,
   ReflectMethod method, cts' ~ (ct ': cts),
   SupportsServantReflex t m,
   Applicative f,
   Traversable f
  ) => HasClientMulti t m (Verb method status cts' a) f tag where

  type ClientMulti t m (Verb method status cts' a) f tag =
    Event t tag -> m (Event t (tag, f (ReqResult a)))

  clientWithRouteMulti _ _ f tag reqs baseurl =
    performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
            reqs' = (\r -> r { reqMethod = method }) <$> reqs

-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method, SupportsServantReflex t m, Traversable f) =>
  HasClientMulti t m (Verb method status cts NoContent) f tag where
  type ClientMulti t m (Verb method status cts NoContent) f tag =
    Event t tag -> m (Event t (tag, f (ReqResult NoContent)))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRouteMulti Proxy _ _ tag req baseurl =
    performRequestsNoBody method req baseurl
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)

instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
    ReflectMethod method, cts' ~ (ct ': cts),
    SupportsServantReflex t m,
    Traversable f
  ) => HasClientMulti t m (Verb method status cts' (Headers ls a)) f tag where
  type ClientMulti t m (Verb method status cts' (Headers ls a)) f tag =
    Event t tag -> m (Event t (tag, f (ReqResult (Headers ls a))))
  clientWithRouteMulti Proxy _ _ _ reqs baseurl triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl triggers :: m (Event t (tag, f (ReqResult a)))
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
  ) => HasClientMulti t m (Verb method status cts (Headers ls NoContent)) f tag where
  type ClientMulti t m (Verb method status cts (Headers ls NoContent)) f tag
    = Event t tag -> m (Event t (tag, f (ReqResult (Headers ls NoContent))))
  clientWithRouteMulti Proxy _ _ _ reqs baseurl triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsNoBody method reqs' baseurl triggers
    return $ (fmap . fmap) toHeaders <$> resp
    where reqs' = ffor reqs $ \req ->
                    req {respHeaders = OnlyHeaders (Set.fromList
                         (buildHeaderKeysTo (Proxy :: Proxy ls)))
                        }


instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          SupportsServantReflex t m,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (Header sym a :> sublayout) f tag where

  type ClientMulti t m (Header sym a :> sublayout) f tag =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy f q tag reqs baseurl eVals =
    clientWithRouteMulti (Proxy :: Proxy sublayout) f
                    q tag
                    reqs'
                    baseurl
    where hname = T.pack $ symbolVal (Proxy :: Proxy sym)
          reqs' = (\eVal req -> Servant.Common.Req.addHeader hname eVal req)
                  <$> eVals <*> reqs


instance HasClientMulti t m sublayout f tag
  => HasClientMulti t m (HttpVersion :> sublayout) f tag where

  type ClientMulti t m (HttpVersion :> sublayout) f tag =
    ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy q f tag =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag


instance (KnownSymbol sym, ToHttpApiData a, HasClientMulti t m sublayout f tag, Reflex t, Traversable f, Applicative f)
      => HasClientMulti t m (QueryParam sym a :> sublayout) f tag where

  type ClientMulti t m (QueryParam sym a :> sublayout) f tag =
    f (Dynamic t (QParam a)) -> ClientMulti t m sublayout f tag

  -- if mparam = Nothing, we don't add it to the query string
  -- TODO: Check the above comment
  clientWithRouteMulti Proxy q f tag reqs baseurl mparams =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag
      reqs' baseurl

    where pname = symbolVal (Proxy :: Proxy sym)
          p prm = QueryPartParam $ fmap qParamToQueryPart prm
          paramPair mp = (T.pack pname, p mp)
          reqs' = (\param req -> req {qParams = paramPair param : qParams req})
                  <$> mparams <*> reqs


instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (QueryParams sym a :> sublayout) f tag where

  type ClientMulti t m (QueryParams sym a :> sublayout) f tag =
    f (Dynamic t [a]) -> ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl paramlists =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl

      where req' l r = r { qParams =  (T.pack pname, params' l) : qParams r }
            pname   = symbolVal (Proxy :: Proxy sym)
            params' l = QueryPartParams $ (fmap . fmap) (toQueryParam)
                        l
            reqs' = req' <$> paramlists <*> reqs


instance (KnownSymbol sym,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (QueryFlag sym :> sublayout) f tag where

  type ClientMulti t m (QueryFlag sym :> sublayout) f tag =
    f (Dynamic t Bool) -> ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl flags =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl

    where req' f req = req { qParams = thisPair f : qParams req }
          thisPair f = (T.pack pName, QueryPartFlag f) :: (Text, QueryPart t)
          pName      = symbolVal (Proxy :: Proxy sym)
          reqs'      = req' <$> flags <*> reqs


instance (SupportsServantReflex t m,
          Traversable f) => HasClientMulti t m Raw f tag where
  type ClientMulti t m Raw f tag = f (Dynamic t (Either Text (XhrRequest ())))
                                 -> Event t tag
                                 -> m (Event t (tag, f (ReqResult ())))

  clientWithRouteMulti _ _ _ _ oldReqs baseurl rawReqs triggers = do
    let rawReqs' = sequence rawReqs :: Dynamic t (f (Either Text (XhrRequest ())))
        rawReqs'' = attachPromptlyDynWith (\fxhr t -> Compose (t, fxhr)) rawReqs' triggers
    resps  <- fmap (second (fmap aux) . getCompose) <$> performSomeRequestsAsync rawReqs''
    return resps
    where
      aux (Right r) = ResponseSuccess () r
      aux (Left  e) = RequestFailure e


instance (MimeRender ct a,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag where

  type ClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl bodies =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl
       where req'        b r = r { reqBody = bodyBytesCT b }
             ctProxy         = Proxy :: Proxy ct
             ctString        = T.pack $ show $ contentType ctProxy
             bodyBytesCT b   = Just $ (fmap . fmap)
                               (\b' -> (mimeRender ctProxy b', ctString))
                               b
             reqs'           = req' <$> bodies <*> reqs


instance (KnownSymbol path,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Functor f) => HasClientMulti t m (path :> sublayout) f tag where
  type ClientMulti t m (path :> sublayout) f tag = ClientMulti t m sublayout f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl =
     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag
                     (prependToPathParts (pure (Right $ T.pack p)) <$> reqs)
                     baseurl

    where p = symbolVal (Proxy :: Proxy path)


instance HasClientMulti t m api f tag => HasClientMulti t m (Vault :> api) f tag where
  type ClientMulti t m (Vault :> api) f tag = ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance HasClientMulti t m api f tag => HasClientMulti t m (RemoteHost :> api) f tag where
  type ClientMulti t m (RemoteHost :> api) f tag = ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance HasClientMulti t m api f tag => HasClientMulti t m (IsSecure :> api) f tag where
  type ClientMulti t m (IsSecure :> api) f tag = ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl =
    clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance (HasClientMulti t m api f tag, Reflex t, Traversable f, Applicative f)
      => HasClientMulti t m (BasicAuth realm usr :> api) f tag where

  type ClientMulti t m (BasicAuth realm usr :> api) f tag = f (Dynamic t (Maybe BasicAuthData))
                                               -> ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f tag reqs baseurl authdatas =
    clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs' baseurl
      where
        req'  a r = r { authData = Just a }
        reqs' = req' <$> authdatas <*> reqs
