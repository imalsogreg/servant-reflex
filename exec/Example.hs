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
  let (getUnit :<|> getInt :<|> doRaw) = client api (Proxy :: Proxy m) (constDyn url)
  b  :: Event t () <- button "Get unit"
  b' :: Event t () <- button "Get int"
  res :: Event t (Maybe (), XhrResponse) <- getUnit b
  res' :: Event t (Maybe Int, XhrResponse) <- getInt b'
  score <- foldDyn (+) 0 (fmapMaybe fst res')
  r <- holdDyn "Waiting" $ leftmost [fmap (showXhrResponse . snd) res
                                    ,fmap (showXhrResponse . snd) res'
                                    ]
  dynText r
  el "br" $ return ()
  text "Total: "
  display score

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
