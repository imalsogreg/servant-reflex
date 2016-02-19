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
url = BaseUrl Http "localhost" 8000 "api"

main :: IO ()
main = mainWidget run

run :: forall t m. MonadWidget t m => m ()
run = do
  let getInt = clientWithRoute api defReq url
  b :: Event t () <- button "Get int"
  res :: Event t ((),Int) <- getInt b
  c <- foldDyn (\n accum -> accum + snd n) 0 $ traceEvent "res" res
  display c

