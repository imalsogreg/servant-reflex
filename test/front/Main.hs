{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Servant.Reflex
import API
import Data.Proxy
import Reflex.Dom
import Reflex.Dom.Contrib.Xhr

api :: Proxy API
api = Proxy

url :: BaseUrl
url = BaseUrl Http "localhost" 8000 ""

main :: IO ()
main = mainWidget run

run :: forall t m. MonadWidget t m => m ()
run = do
  let (getUnit :<|> get100) = client api url
  b :: Event t () <- button "Get unit"
  res :: Event t ((),()) <- getUnit b
  c <- foldDyn (\_ (n :: Int) -> succ n) 0 res
  display c

