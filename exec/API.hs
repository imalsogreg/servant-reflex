{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

-- | API spec for server, client, and docs
type API = "getunit" :> Get '[JSON] ()
      :<|> "getint"  :> Get '[JSON] Int
      :<|> "sayhi"   :> QueryParam "username" String :> Get '[JSON] String
      :<|> Raw
