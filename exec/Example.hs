{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Bool
import Data.Maybe
import Servant.API
import Servant.Reflex
import API
import Data.Proxy
import Text.Read (readMaybe)
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
  let (getUnit :<|> getInt :<|> sayhi :<|> dbl :<|> doRaw) =
        client api (Proxy :: Proxy m) (constDyn url)

  elClass "div" "demo-group" $ do
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

  elClass "div" "demo-group" $ do

    text "Name"
    el "br" $ return ()
    inp :: Dynamic t String <- fmap value (textInput def)
    let checkedName = fmap (\i -> bool (Just i) Nothing (null i)) (current inp)
    el "br" $ return ()

    text "Greetings (space-separated)"
    el "br" $ return ()
    greetings <- fmap (fmap words . current . value) (textInput def)

    el "br" $ return ()

    gusto <- value <$> checkbox False def

    el "br" $ return ()
    sayhiClicks :: Event t () <- button "Say hi"

    resp <- fmap fst <$> sayhi checkedName greetings (current gusto) sayhiClicks
    dynText =<< holdDyn "No hi yet" (fmapMaybe id resp)

  elClass "div" "demo-group" $ do
    text "A Double to double"
    el "br" $ return ()
    dblinp <- value <$> textInput def
    dblBtn <- button "Double it"
    dblResp :: Event t (Maybe Double) <- fmap fst <$> dbl (fmap readMaybe $ current dblinp) dblBtn
    display =<< holdDyn "No number yet" (fmap show $ fmapMaybe id dblResp)

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
