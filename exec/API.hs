{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

-- | API spec for server, client, and docs
type API = "getunit" :> Get '[JSON] ()
      :<|> "getint"  :> Get '[JSON] Int
      :<|> Raw
