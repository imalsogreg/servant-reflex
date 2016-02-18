{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import API
import Data.Proxy
import Reflex.Dom
import Servant.Reflex

api :: Proxy API
api = Proxy

url :: BaseUrl
url = BaseUrl Http "localhost" 8000 ""

main :: IO ()
main = mainWidget run

run :: forall t m. MonadWidget t m => m ()
run = do
  let getUnit = clientWithRoute api defReq url
  b :: Event t () <- button "Get unit"
  res :: Event t ((),()) <- getUnit b
  c <- foldDyn (\_ (n :: Int) -> succ n) 0 res
  display c

