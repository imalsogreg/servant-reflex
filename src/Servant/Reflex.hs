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
module Servant.Reflex
  ( client
  , HasClient(..)
  , module Servant.Common.Req
  , module Servant.Common.BaseUrl
  ) where

import           Control.Applicative        ((<$>), liftA2)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (unpack)
import           Data.Traversable           (sequenceA)
import           GHC.TypeLits
import qualified Network.HTTP.Media         as M
import           Servant.API
import           Servant.Common.BaseUrl
import           Servant.Common.Req
import           Servant.API.ContentTypes
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Xhr

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: Event t () -> m (Event t (Either XhrError ((),[Book])))
-- > postNewBook :: Behavior t (Maybe Book) -> Event t ()
--               -> m (Event t (Either XhrError (Book,Book)))
-- > (getAllBooks :<|> postNewBook) = client myApi host
-- >   where host = constDyn $ BaseUrl Http "localhost" 8080
client :: (HasClient t m layout, MonadWidget t m)
       => Proxy layout -> Proxy m -> Dynamic t BaseUrl -> Client t m layout
client p q baseurl = clientWithRoute p q defReq baseurl

-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient t m layout where
  type Client t m layout :: *
  clientWithRoute :: Proxy layout -> Proxy m -> Req t -> Dynamic t BaseUrl -> Client t m layout


instance (HasClient t m a, HasClient t m b) => HasClient t m (a :<|> b) where
  type Client t m (a :<|> b) = Client t m a :<|> Client t m b
  clientWithRoute Proxy q req baseurl =
    clientWithRoute (Proxy :: Proxy a) q req baseurl :<|>
    clientWithRoute (Proxy :: Proxy b) q req baseurl

-- Capture. Example:
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > myApi :: Proxy MyApi = Proxy
-- >
-- > getBook :: MonadWidget t m
--           => Dynamic t BaseUrl
--           -> Behavior t (Maybe Text)
--           -> Event t ()
--           -> m (Event t (Either XhrError (Text, Book)))
-- > getBook = client myApi (constDyn host)
instance (MonadWidget t m, KnownSymbol capture, ToHttpApiData a, HasClient t m sublayout)
      => HasClient t m (Capture capture a :> sublayout) where

  type Client t m (Capture capture a :> sublayout) =
    Behavior t (Either String a) -> Client t m sublayout

  clientWithRoute Proxy q req baseurl val =
    clientWithRoute (Proxy :: Proxy sublayout)
                    q
                    (prependToPathParts p req)
                    baseurl

    where p = (fmap . fmap) (unpack . toUrlPiece) val

-- VERB (Returning content) --
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts), MonadWidget t m
  ) => HasClient t m (Verb method status cts' a) where
  type Client t m (Verb method status cts' a) =
    Event t () -> m (Event t (ReqResult a))
    -- TODO how to access input types here?
    -- ExceptT ServantError IO a
  clientWithRoute Proxy q req baseurl =
    performRequestCT (Proxy :: Proxy ct) method req' baseurl
      where method = BS.unpack $ reflectMethod (Proxy :: Proxy method)
            req' = req { reqMethod = method }

-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method, MonadWidget t m) =>
  HasClient t m (Verb method status cts NoContent) where
  type Client t m (Verb method status cts NoContent) =
    Event t () -> m (Event t (ReqResult NoContent))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRoute Proxy q req baseurl =
    performRequestNoBody method req baseurl
      where method = BS.unpack $ reflectMethod (Proxy :: Proxy method)

-- -- HEADERS Verb (Content) --
-- -- Headers combinator not treated in fully general case,
-- -- in order to deny instances for (Headers ls (Capture "id" Int)),
-- -- a combinator that wouldn't make sense
-- -- TODO Overlapping??
-- instance {-# OVERLAPPABLE #-}
--   -- Note [Non-Empty Content Types]
--   ( MimeUnrender ct a, BuildHeadersTo ls,
--     ReflectMethod method, cts' ~ (ct ': cts),
--     MonadWidget t m
--   ) => HasClient t m (Verb method status cts' (Headers ls a)) where
--   type Client t m (Verb method status cts' (Headers ls a))
--     = Event t () -> m (Event t (Maybe a, XhrResponse))
--       -- ExceptT ServantError IO (Headers ls a)
--   clientWithRoute Proxy req baseurl = do
--     let method = reflectMethod (Proxy :: Proxy method)
--     (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) method req baseurl
--     return $ Headers { getResponse = resp
--                      , getHeadersHList = buildHeadersTo hdrs
--                      }

-- TODO Overlapping??
-- -- HEADERS Verb (No content) --
-- instance {-# OVERLAPPABLE #-}
--   ( BuildHeadersTo ls, ReflectMethod method,
--     MonadWidget t m
--   ) => HasClient t m (Verb method status cts (Headers ls NoContent)) where
--   type Client t m (Verb method status cts (Headers ls NoContent))
--     = Event t () -> m (Event t XhrResponse)
--       -- ExceptT ServantError IO (Headers ls NoContent)
--   clientWithRoute Proxy req baseurl = do
--     let method = reflectMethod (Proxy :: Proxy method)
--     hdrs <- performRequestNoBody method req baseurl
--     return $ Headers { getResponse = NoContent
--                      , getHeadersHList = buildHeadersTo hdrs
--                      }


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
          HasClient t m sublayout, MonadWidget t m)
      => HasClient t m (Header sym a :> sublayout) where

  type Client t m (Header sym a :> sublayout) =
    Behavior t (Either String a) -> Client t m sublayout

  clientWithRoute Proxy q req baseurl mval =
    clientWithRoute (Proxy :: Proxy sublayout)
                    q
                    req
                    -- (maybe req -- TODO Need to pass the header in
                    --        (\value -> Servant.Common.Req.addHeader hname value req)
                    --        mval
                    -- )
                    baseurl

    where hname = symbolVal (Proxy :: Proxy sym)

-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance HasClient t m sublayout
  => HasClient t m (HttpVersion :> sublayout) where

  type Client t m (HttpVersion :> sublayout) =
    Client t m sublayout

  clientWithRoute Proxy q =
    clientWithRoute (Proxy :: Proxy sublayout) q

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
instance (KnownSymbol sym, ToHttpApiData a, HasClient t m sublayout, Reflex t)
      => HasClient t m (QueryParam sym a :> sublayout) where

  type Client t m (QueryParam sym a :> sublayout) =
    -- TODO (Maybe a), or (Maybe (Maybe a))? (should the user be able to send a Nothing)
    Behavior t (Either String a) -> Client t m sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy q req baseurl mparam =
    clientWithRoute (Proxy :: Proxy sublayout) q
      (req {qParams = paramPair : qParams req}) baseurl

    where pname = symbolVal (Proxy :: Proxy sym)
          p prm = QueryPartParam $ (fmap . fmap) (unpack . toQueryParam) prm
          paramPair = (pname, p mparam)

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
instance (KnownSymbol sym, ToHttpApiData a, HasClient t m sublayout, Reflex t)
      => HasClient t m (QueryParams sym a :> sublayout) where

  type Client t m (QueryParams sym a :> sublayout) =
    Behavior t [a] -> Client t m sublayout

  clientWithRoute Proxy q req baseurl paramlist =
    clientWithRoute (Proxy :: Proxy sublayout) q req' baseurl

      where req'    = req { qParams =  (pname, params') : qParams req }
            pname   = symbolVal (Proxy :: Proxy sym)
            params' = QueryPartParams $ (fmap . fmap) (unpack . toQueryParam)
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
instance (KnownSymbol sym, HasClient t m sublayout, Reflex t)
      => HasClient t m (QueryFlag sym :> sublayout) where

  type Client t m (QueryFlag sym :> sublayout) =
    Behavior t Bool -> Client t m sublayout

  clientWithRoute Proxy q req baseurl flag =
    clientWithRoute (Proxy :: Proxy sublayout) q req' baseurl

    where req'     = req { qParams = thisPair : qParams req }
          thisPair = (pName, QueryPartFlag flag) :: (String, QueryPart t)
          pName    = symbolVal (Proxy :: Proxy sym)


-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
-- TODO redo
instance (MonadWidget t m) => HasClient t m Raw where
  type Client t m Raw = Behavior t (Either String XhrRequest)
                      -> Event t ()
                      -> m (Event t (ReqResult ()))

  -- clientWithRoute :: Proxy Raw -> Proxy m -> Req -> BaseUrl -> Client t m Raw
  clientWithRoute p q req baseurl xhrs triggers = do
    let xhrs'  = liftA2 (\x path -> case x of
                    Left e  -> Left e
                    Right jx -> Right $ jx {_xhrRequest_url = path
                                            ++ _xhrRequest_url jx})
                 xhrs
                 (showBaseUrl <$> current baseurl)
        reqs   = tag xhrs' triggers
        okReq  = fmapMaybe hush reqs
        badReq = fmapMaybe tattle reqs
    resps <- performRequestAsync okReq
    return $ leftmost [fmap (ResponseSuccess () ) resps
                      ,fmap RequestFailure badReq]


hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

tattle :: Either e a -> Maybe e
tattle = either Just (const Nothing)

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

instance (MimeRender ct a, HasClient t m sublayout, Reflex t)
      => HasClient t m (ReqBody (ct ': cts) a :> sublayout) where

  type Client t m (ReqBody (ct ': cts) a :> sublayout) =
    Behavior t (Either String a) -> Client t m sublayout

  clientWithRoute Proxy q req baseurl body =
    clientWithRoute (Proxy :: Proxy sublayout) q req' baseurl
       where req'        = req { reqBody = bodyBytesCT }
             ctProxy     = Proxy :: Proxy ct
             ctString    = show $ contentType ctProxy
             --ctString    = BS.unpack . CI.original . M.mainType $ contentType ctProxy
             bodyBytesCT = Just $ (fmap . fmap)
                             (\b -> (mimeRender ctProxy b, ctString))
                             body

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient t m sublayout, Reflex t) => HasClient t m (path :> sublayout) where
  type Client t m (path :> sublayout) = Client t m sublayout

  clientWithRoute Proxy q req baseurl =
     clientWithRoute (Proxy :: Proxy sublayout) q
                     (prependToPathParts (constant (Right p)) req)
                     baseurl

    where p = symbolVal (Proxy :: Proxy path)

instance HasClient t m api => HasClient t m (Vault :> api) where
  type Client t m (Vault :> api) = Client t m api

  clientWithRoute Proxy q req baseurl =
    clientWithRoute (Proxy :: Proxy api) q req baseurl

instance HasClient t m api => HasClient t m (RemoteHost :> api) where
  type Client t m (RemoteHost :> api) = Client t m api

  clientWithRoute Proxy q req baseurl =
    clientWithRoute (Proxy :: Proxy api) q req baseurl

instance HasClient t m api => HasClient t m (IsSecure :> api) where
  type Client t m (IsSecure :> api) = Client t m api

  clientWithRoute Proxy q req baseurl =
    clientWithRoute (Proxy :: Proxy api) q req baseurl

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
