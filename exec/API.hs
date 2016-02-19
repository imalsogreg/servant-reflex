{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

--type API = Get '[JSON] () :<|> Get '[JSON] Int
type API = Get '[JSON] Int
