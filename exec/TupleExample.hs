{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications #-}

module TupleExample where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.TypeLits
import Reflex.Dom
import Servant.API
import Servant.Reflex.Tuple

type TestAPI = Get '[JSON] Int
          -- :<|> Post '[JSON] Int
          :<|> Capture "name" Text :> Post '[JSON] Text


run :: forall t m. (SupportsServantReflex t m, MonadWidget t m) => m ()
run = do
    let (getInt :<|> greet) = client (Proxy @TestAPI) (Proxy @m) (BasePath "/")
    -- let getInt = client (Proxy @TestAPI) (Proxy @m) (BasePath "/")
    b <- button "Go for it"
    g <- ("greg" <$) <$> button "Hello"
    -- g <- button "Hello"
    b' <- getInt b
    display =<< holdDyn (Left "Waiting") b'
    g' <- greet g
    display =<< holdDyn (Left "Waiting") g'

