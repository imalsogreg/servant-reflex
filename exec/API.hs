{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

type API = "getunit" :> Get '[JSON] () 
      :<|> "getint"  :> Get '[JSON] Int
      :<|> Raw
