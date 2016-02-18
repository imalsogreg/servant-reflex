{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Maybe
import Servant.API
import Servant.Reflex
import API
import Data.Proxy
import Reflex.Dom

api :: Proxy API
api = Proxy

url :: BaseUrl
url = BaseUrl Http "localhost" 8000 ""

main :: IO ()
main = mainWidget run


run :: forall t m. MonadWidget t m => m ()
run = do

  -- Name the computed API client functions
  let (getUnit :<|> getInt :<|> doRaw) =
        client api (Proxy :: Proxy m) (constDyn url)

  unitBtn  <- button "Get unit"
  intBtn   <- button "Get int"

  unitResponse <- getUnit unitBtn
  intResponse :: Event t (Maybe Int, XhrResponse) <- getInt intBtn

  score <- foldDyn (+) 0 (fmapMaybe fst intResponse)

  r <- holdDyn "Waiting" $
         leftmost [fmap (showXhrResponse . snd) unitResponse
                  ,fmap (showXhrResponse . snd) intResponse
                  ]
  dynText r >> el "br" (return ()) >> text "Total: " >> display score

showXhrResponse :: XhrResponse -> String
showXhrResponse (XhrResponse stat stattxt rbmay rtmay) =
  unlines ["stat: " ++ show stat
          ,"stattxt: " ++ show stattxt
          ,"resp: " ++ maybe "" showRB rbmay
          ,"rtext: " ++ show rtmay]

showRB :: XhrResponseBody -> String
showRB (XhrResponseBody_Default t) = show t
showRB (XhrResponseBody_Text t) = show t
showRB (XhrResponseBody_Blob t) = "<Blob>"
showRB (XhrResponseBody_ArrayBuffer t) = show t
