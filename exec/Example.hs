{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Servant.API
import Servant.Reflex
import API
import Data.Proxy
import Text.Read (readMaybe)
import Reflex.Dom

api :: Proxy API
api = Proxy

main :: IO ()
main = mainWidget run


run :: forall t m. MonadWidget t m => m ()
run = do

  -- Allow user to choose the url target for the request
  -- (alternatively we could just `let url = constDyn (BasePath "/")`)
  url <- baseUrlWidget
  el "br" (return ())
  dynText =<< mapDyn showBaseUrl url
  el "br" (return ())

  -- Name the computed API client functions
  let (getUnit :<|> getInt :<|> sayhi :<|> dbl :<|> multi :<|> doRaw) =
        client api (Proxy :: Proxy m) url

  elClass "div" "demo-group" $ do
    unitBtn  <- button "Get unit"
    intBtn   <- button "Get int"

    unitResponse <- getUnit unitBtn
    intResponse :: Event t (ReqResult Int) <- getInt intBtn

    score <- foldDyn (+) 0 (fmapMaybe reqSuccess intResponse)

    r <- holdDyn "Waiting" $ fmap showXhrResponse $
         leftmost [fmapMaybe response unitResponse
                  ,fmapMaybe response intResponse
                  ]
    dynText r >> el "br" (return ()) >> text "Total: " >> display score

  elClass "div" "demo-group" $ do

    text "Name"
    el "br" $ return ()
    inp :: Dynamic t Text <- fmap value (textInput def)
    let checkedName = fmap (\i -> bool (Right i) (Left "Need a name") (T.null i)) (current inp)
    el "br" $ return ()

    text "Greetings (space-separated)"
    el "br" $ return ()
    greetings <- fmap (fmap T.words . current . value) (textInput def)

    el "br" $ return ()

    gusto <- value <$> checkbox False def

    el "br" $ return ()
    sayhiClicks :: Event t () <- button "Say hi"

    resp <- sayhi checkedName greetings (current gusto) sayhiClicks
    dynText =<< holdDyn "No hi yet" (leftmost [fmapMaybe reqSuccess resp, fmapMaybe reqFailure resp])

  elClass "div" "demo-group" $ do
    text "A Double to double"
    el "br" $ return ()
    dblinp <- value <$> textInput def
    dblBtn <- button "Double it"
    dblResp <- dbl (fmap (note "read failure" . readMaybe . T.unpack) $ current dblinp) dblBtn
    dynText =<< holdDyn "(no errors)" (fmapMaybe reqFailure dblResp)
    el "br" (return ())
    display =<< holdDyn "No number yet" (fmap tShow $ fmapMaybe reqSuccess dblResp)

  elClass "div" "demo-group" $ do
    text "Multi-part path"
    b <- (current . value) <$> checkbox False def
    mpGo <- button "Test"
    multiResp <- multi b mpGo
    dynText =<< holdDyn "No res yet" (fmap tShow $ fmapMaybe reqSuccess $ multiResp)

showXhrResponse :: XhrResponse -> Text
showXhrResponse (XhrResponse stat stattxt rbmay rtmay) =
  T.unlines ["stat: " <> tShow stat
            ,"stattxt: " <> tShow stattxt
            ,"resp: " <> maybe "" showRB rbmay
            ,"rtext: " <> tShow rtmay]

tShow :: Show a => a -> Text
tShow = T.pack . show

showRB :: XhrResponseBody -> Text
showRB (XhrResponseBody_Default t) = tShow t
showRB (XhrResponseBody_Text t) = tShow t
showRB (XhrResponseBody_Blob t) = "<Blob>"
showRB (XhrResponseBody_ArrayBuffer t) = tShow t
