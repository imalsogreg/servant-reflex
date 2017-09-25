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

-- #include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Reflex
  ( client
  , clientWithOpts
  , clientWithRoute
  , BuildHeaderKeysTo(..)
  , toHeaders
  , HasClient
  , Client
  , withCredentials
  , module Servant.Common.Req
  , module Servant.Common.BaseUrl
  ) where

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

import           Reflex.Dom              (Dynamic, Event, Reflex,
                                          XhrRequest(..),
                                          XhrResponseHeaders(..),
                                          XhrResponse(..), attachPromptlyDynWith, constDyn, ffor, fmapMaybe,
                                          leftmost, performRequestsAsync,
                                          )
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)
import           Servant.Common.Req      (ClientOptions(..),
                                          defaultClientOptions,
                                          Req, ReqResult(..), QParam(..),
                                          QueryPart(..), addHeader, authData,
                                          defReq, evalResponse, prependToPathParts,
                                          -- performRequestCT,
                                          performRequestsCT,
                                          -- performRequestNoBody,
                                          performRequestsNoBody,
                                          performSomeRequestsAsync,
                                          qParamToQueryPart, reqBody,
                                          reqSuccess, reqFailure,
                                          reqMethod, respHeaders,
                                          response,
                                          reqTag,
                                          qParams, withCredentials)


-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: Event t l -> m (Event t (l, ReqResult [Book]))
-- > postNewBook :: Dynamic t (Maybe Book) -> Event t l
--               -> m (Event t (l, ReqResult Book)))
-- > (getAllBooks :<|> postNewBook) = client myApi host
-- >   where host = constDyn $ BaseUrl Http "localhost" 8080
client
    :: (HasClient t m layout tag)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> Client t m layout tag
client p q t baseurl = clientWithRoute p q t defReq baseurl defaultClientOptions

clientWithOpts
    :: (HasClient t m layout tag)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Client t m layout tag
clientWithOpts p q t baseurl = clientWithRoute p q t defReq baseurl


-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient t m layout (tag :: *) where
  type Client t m layout tag :: *
  clientWithRoute :: Proxy layout -> Proxy m -> Proxy tag -> Req t -> Dynamic t BaseUrl -> ClientOptions -> Client t m layout tag


instance (HasClient t m a tag, HasClient t m b tag) => HasClient t m (a :<|> b) tag where
  type Client t m (a :<|> b) tag = Client t m a tag :<|> Client t m b tag

  clientWithRoute Proxy q pTag req baseurl opts =
    clientWithRoute (Proxy :: Proxy a) q pTag req baseurl opts :<|>
    clientWithRoute (Proxy :: Proxy b) q pTag req baseurl opts


-- Capture. Example:
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > myApi :: Proxy MyApi = Proxy
-- >
-- > getBook :: SupportsServantReflex t m
--           => Dynamic t BaseUrl
--           -> Dynamic t (Maybe Text)
--           -> Event t l
--           -> m (Event t (l, ReqResult Book))
-- > getBook = client myApi (constDyn host)

instance (SupportsServantReflex t m, ToHttpApiData a, HasClient t m sublayout tag)
      => HasClient t m (Capture capture a :> sublayout) tag where

  type Client t m (Capture capture a :> sublayout) tag =
    Dynamic t (Either Text a) -> Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts val =
    clientWithRoute (Proxy :: Proxy sublayout)
                    q t
                    (prependToPathParts p req)
                    baseurl opts
    where p = (fmap . fmap) (toUrlPiece) val


-- VERB (Returning content) --
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts), SupportsServantReflex t m
  ) => HasClient t m (Verb method status cts' a) tag where
  type Client t m (Verb method status cts' a) tag =
    Event t tag -> m (Event t (ReqResult tag a))
    -- TODO how to access input types here?
    -- ExceptT ServantError IO a
  clientWithRoute Proxy _ _ req baseurl opts trigs =
      fmap runIdentity <$> performRequestsCT (Proxy :: Proxy ct) method (constDyn $ Identity $ req') baseurl opts trigs
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
            req' = req { reqMethod = method }


-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method, SupportsServantReflex t m) =>
  HasClient t m (Verb method status cts NoContent) tag where
  type Client t m (Verb method status cts NoContent) tag =
    Event t tag -> m (Event t (ReqResult tag NoContent))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRoute Proxy _ _ req baseurl opts =
    (fmap . fmap) runIdentity . performRequestsNoBody method (constDyn $ Identity req) baseurl opts
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)


toHeaders :: BuildHeadersTo ls => ReqResult tag a -> ReqResult tag (Headers ls a)
toHeaders r =
  let toBS = E.encodeUtf8
      hdrs = maybe []
                   (\xhr -> fmap (\(h,v) -> (mk (toBS h), toBS v))
                     (Map.toList $ _xhrResponse_headers xhr))
                   (response r)
  in  ffor r $ \a -> Headers {getResponse = a ,getHeadersHList = buildHeadersTo hdrs}

class BuildHeaderKeysTo hs where
  buildHeaderKeysTo :: Proxy hs -> [T.Text]

instance {-# OVERLAPPABLE #-} BuildHeaderKeysTo '[]
  where buildHeaderKeysTo _ = []

instance {-# OVERLAPPABLE #-} (BuildHeaderKeysTo xs, KnownSymbol h)
  => BuildHeaderKeysTo ((Header h v) ': xs) where
  buildHeaderKeysTo _ = T.pack (symbolVal (Proxy :: Proxy h)) : buildHeaderKeysTo (Proxy :: Proxy xs)

-- HEADERS Verb (Content) --
-- Headers combinator not treated in fully general case,
-- in order to deny instances for (Headers ls (Capture "id" Int)),
-- a combinator that wouldn't make sense
-- TODO Overlapping??
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
    ReflectMethod method, cts' ~ (ct ': cts),
    SupportsServantReflex t m
  ) => HasClient t m (Verb method status cts' (Headers ls a)) tag where
  type Client t m (Verb method status cts' (Headers ls a)) tag =
      Event t tag -> m (Event t (ReqResult tag (Headers ls a)))
  clientWithRoute Proxy _ _ req baseurl opts trigs = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- fmap runIdentity <$> performRequestsCT (Proxy :: Proxy ct) method (constDyn $ Identity req') baseurl opts trigs
    return $ toHeaders <$> resp
    where req' = req { respHeaders =
                       OnlyHeaders (Set.fromList (buildHeaderKeysTo (Proxy :: Proxy ls)))
                     }


-- HEADERS Verb (No content) --
instance {-# OVERLAPPABLE #-}
  ( BuildHeadersTo ls, BuildHeaderKeysTo ls, ReflectMethod method,
    SupportsServantReflex t m
  ) => HasClient t m (Verb method status cts (Headers ls NoContent)) tag where
  type Client t m (Verb method status cts (Headers ls NoContent)) tag
    = Event t tag -> m (Event t (ReqResult tag (Headers ls NoContent)))
  clientWithRoute Proxy _ _ req baseurl opts trigs = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- fmap runIdentity <$> performRequestsNoBody method (constDyn $ Identity req') baseurl opts trigs
    return $ toHeaders <$> resp
    where req' = req {respHeaders =
                      OnlyHeaders (Set.fromList (buildHeaderKeysTo (Proxy :: Proxy ls)))
                     }



-- HEADER
-- > newtype Referer = Referer { referrer :: Text }
-- >   deriving (Eq, Show, Generic, FromText, ToHttpApiData)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- >
-- > viewReferer :: Maybe Referer -> ExceptT String IO Book
-- > viewReferer = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or e.g Just "http://haskell.org/" as arguments
instance (KnownSymbol sym, ToHttpApiData a,
          HasClient t m sublayout tag, SupportsServantReflex t m)
      => HasClient t m (Header sym a :> sublayout) tag where

  type Client t m (Header sym a :> sublayout) tag =
    Dynamic t (Either Text a) -> Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts eVal =
    clientWithRoute (Proxy :: Proxy sublayout)
                    q t
                    (Servant.Common.Req.addHeader hname eVal req)
                    baseurl opts
    where hname = T.pack $ symbolVal (Proxy :: Proxy sym)




-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance HasClient t m sublayout tag
  => HasClient t m (HttpVersion :> sublayout) tag where

  type Client t m (HttpVersion :> sublayout) tag =
    Client t m sublayout tag

  clientWithRoute Proxy q t =
    clientWithRoute (Proxy :: Proxy sublayout) q t


-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToHttpApiData a, HasClient t m sublayout tag, Reflex t)
      => HasClient t m (QueryParam sym a :> sublayout) tag where

  type Client t m (QueryParam sym a :> sublayout) tag =
    Dynamic t (QParam a) -> Client t m sublayout tag

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy q t req baseurl opts mparam =
    clientWithRoute (Proxy :: Proxy sublayout) q t
      (req {qParams = paramPair : qParams req}) baseurl opts

    where pname = symbolVal (Proxy :: Proxy sym)
          --p prm = QueryPartParam $ (fmap . fmap) (toQueryParam) prm
          --paramPair = (T.pack pname, p mparam)
          p prm = QueryPartParam $ fmap qParamToQueryPart prm -- (fmap . fmap) (unpack . toQueryParam) prm
          paramPair = (T.pack pname, p mparam)



-- | If you use a 'QueryParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument, a list of values of the type specified
-- by your 'QueryParams'.
--
-- If you give an empty list, nothing will be added to the query string.
--
-- Otherwise, this function will take care
-- of inserting a textual representation of your values in the query string,
-- under the same query string parameter name.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToHttpApiData a, HasClient t m sublayout tag, Reflex t)
      => HasClient t m (QueryParams sym a :> sublayout) tag where

  type Client t m (QueryParams sym a :> sublayout) tag =
    Dynamic t [a] -> Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts paramlist =
    clientWithRoute (Proxy :: Proxy sublayout) q t req' baseurl opts

      where req'    = req { qParams =  (T.pack pname, params') : qParams req }
            pname   = symbolVal (Proxy :: Proxy sym)
            params' = QueryPartParams $ (fmap . fmap) toQueryParam
                        paramlist



-- | If you use a 'QueryFlag' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional 'Bool' argument.
--
-- If you give 'False', nothing will be added to the query string.
--
-- Otherwise, this function will insert a value-less query string
-- parameter under the name associated to your 'QueryFlag'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> ExceptT String IO [Book]
-- > getBooks = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books

-- TODO Bring back
instance (KnownSymbol sym, HasClient t m sublayout tag, Reflex t)
      => HasClient t m (QueryFlag sym :> sublayout) tag where

  type Client t m (QueryFlag sym :> sublayout) tag =
    Dynamic t Bool -> Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts flag =
    clientWithRoute (Proxy :: Proxy sublayout) q t req' baseurl opts

    where req'     = req { qParams = thisPair : qParams req }
          thisPair = (T.pack pName, QueryPartFlag flag) :: (Text, QueryPart t)
          pName    = symbolVal (Proxy :: Proxy sym)



-- | Send a raw 'XhrRequest ()' directly to 'baseurl'
instance SupportsServantReflex t m => HasClient t m Raw tag where
  type Client t m Raw tag = Dynamic t (Either Text (XhrRequest ()))
                      -> Event t tag
                      -> m (Event t (ReqResult tag ()))

  clientWithRoute _ _ _ _ baseurl _ xhrs triggers = do

    let xhrs'   = liftA2 (\x path -> case x of
                             Left e -> Left e
                             Right jx -> Right $ jx { _xhrRequest_url = path <> _xhrRequest_url jx }
                         ) xhrs (showBaseUrl <$> baseurl)
        xhrs''  = attachPromptlyDynWith (flip (,)) xhrs' triggers :: Event t (tag, Either Text (XhrRequest ()))
        badReq = fmapMaybe (\(t,x) -> either (Just . (t,)) (const Nothing) x) xhrs'' :: Event t (tag, Text)
        okReq  = fmapMaybe (\(t,x) -> either (const Nothing) (Just . (t,)) x) xhrs'' :: Event t (tag, XhrRequest ())

    resps  <- performRequestsAsync okReq
    return $ leftmost [ uncurry RequestFailure <$> badReq
                      , evalResponse (const $ Right ()) <$> resps
                      ]


-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> ExceptT String IO Book
-- > addBook = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "addBook" to query that endpoint

instance (MimeRender ct a, HasClient t m sublayout tag, Reflex t)
      => HasClient t m (ReqBody (ct ': cts) a :> sublayout) tag where

  type Client t m (ReqBody (ct ': cts) a :> sublayout) tag =
    Dynamic t (Either Text a) -> Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts body =
    clientWithRoute (Proxy :: Proxy sublayout) q t req' baseurl opts
       where req'        = req { reqBody = bodyBytesCT }
             ctProxy     = Proxy :: Proxy ct
             ctString    = T.pack $ show $ contentType ctProxy
             bodyBytesCT = Just $ (fmap . fmap)
                             (\b -> (mimeRender ctProxy b, ctString))
                             body



-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient t m sublayout tag, Reflex t) => HasClient t m (path :> sublayout) tag where
  type Client t m (path :> sublayout) tag = Client t m sublayout tag

  clientWithRoute Proxy q t req baseurl opts =
     clientWithRoute (Proxy :: Proxy sublayout) q t
                     (prependToPathParts (pure (Right $ T.pack p)) req) baseurl opts

    where p = symbolVal (Proxy :: Proxy path)


instance HasClient t m api tag => HasClient t m (Vault :> api) tag where
  type Client t m (Vault :> api) tag = Client t m api tag

  clientWithRoute Proxy q t req baseurl =
    clientWithRoute (Proxy :: Proxy api) q t req baseurl


instance HasClient t m api tag => HasClient t m (RemoteHost :> api) tag where
  type Client t m (RemoteHost :> api) tag = Client t m api tag

  clientWithRoute Proxy q t req baseurl =
    clientWithRoute (Proxy :: Proxy api) q t req baseurl



instance HasClient t m api tag => HasClient t m (IsSecure :> api) tag where
  type Client t m (IsSecure :> api) tag = Client t m api tag

  clientWithRoute Proxy q t req baseurl =
    clientWithRoute (Proxy :: Proxy api) q t req baseurl


instance (HasClient t m api tag, Reflex t)
      => HasClient t m (BasicAuth realm usr :> api) tag where

  type Client t m (BasicAuth realm usr :> api) tag = Dynamic t (Maybe BasicAuthData)
                                               -> Client t m api tag

  clientWithRoute Proxy q t req baseurl opts authdata =
    clientWithRoute (Proxy :: Proxy api) q t req' baseurl opts
      where
        req'    = req { authData = Just authdata }

-- instance HasClient t m subapi =>
--   HasClient t m (WithNamedConfig name config subapi) where

--   type Client t m (WithNamedConfig name config subapi) = Client t m subapi
--   clientWithRoute Proxy q = clientWithRoute (Proxy :: Proxy subapi) q


{- Note [Non-Empty Content Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rather than have

   instance (..., cts' ~ (ct ': cts)) => ... cts' ...

It may seem to make more sense to have:

   instance (...) => ... (ct ': cts) ...

But this means that if another instance exists that does *not* require
non-empty lists, but is otherwise more specific, no instance will be overall
more specific. This in turn generally means adding yet another instance (one
for empty and one for non-empty lists).
-}


-- SUPPORT FOR servant-auth --

-- For JavaScript clients we should be sending/storing JSON web tokens in a
-- way that is inaccessible to JavaScript.
--
-- For @servant-auth@ this is done with HTTP-only cookies. In a Reflex-DOM
-- app this means the @servant-auth@ client should only verify that the API
-- supports Cookie-based authentication but do nothing with the token
-- directly.

-- @HasCookieAuth auths@ is nominally a redundant constraint, but ensures
-- we're not trying to rely on cookies when the API does not use them.
instance (HasCookieAuth auths, HasClient t m api tag) => HasClient t m (Auth.Auth auths a :> api) tag where

  type Client t m (Auth.Auth auths a :> api) tag = Client t m api tag
  clientWithRoute Proxy = clientWithRoute (Proxy :: Proxy api)


type family HasCookieAuth xs :: Constraint where
  HasCookieAuth (Auth.Cookie ': xs) = ()
  HasCookieAuth (x ': xs)   = HasCookieAuth xs
  HasCookieAuth '[]         = CookieAuthNotEnabled

class CookieAuthNotEnabled

