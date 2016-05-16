{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

-- | API spec for server, client, and docs
type API = "getunit" :> Get '[JSON] ()
      :<|> "getint"  :> Get '[JSON] Int
      :<|> "sayhi"   :> QueryParam  "username" String
                     :> QueryParams "greetings" String
                     :> QueryFlag   "gusto"
                     :> Get '[JSON] String
      :<|> "double" :> ReqBody '[JSON] Double
                    :> Post '[JSON] Double
      :<|> "a" :> "b" :> QueryFlag "gusto" :> Get '[JSON] String
      :<|> Raw

type GET = Get '[JSON] ()

-- Imported the comprehensive API example for testing.
-- https://github.com/haskell-servant/servant/blob/master/servant/src/Servant/API/Internal/Test/ComprehensiveAPI.hs
type ComprehensiveAPI =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
-- Raw :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
  Get '[JSON] (Headers '[Header "foo" Int] ()) :<|>
  "foo" :> GET :<|>
  Vault :> GET :<|>
  Verb 'POST 204 '[JSON] () :<|>
  Verb 'POST 204 '[JSON] Int
-- This one isn't in scope
--  :<|> WithNamedContext "foo" '[] GET
