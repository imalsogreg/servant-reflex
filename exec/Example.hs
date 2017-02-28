{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool
import Data.Maybe
import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))
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


run :: forall t m. (SupportsServantReflex t m,
                    DomBuilder t m,
                    DomBuilderSpace m ~ GhcjsDomSpace,
                    MonadFix m,
                    PostBuild t m,
                    MonadHold t m) => m ()
run = do

  -- Allow user to choose the url target for the request
  -- (alternatively we could just `let url = constDyn (BasePath "/")`)
  url <- baseUrlWidget
  el "br" (return ())
  dynText $ showBaseUrl <$> url
  el "br" (return ())

  -- Name the computed API client functions
  let (getUnit :<|> getInt :<|> sayhi :<|> dbl :<|> multi :<|> qna :<|> doRaw) =
        client api (Proxy :: Proxy m) url

  elClass "div" "demo-group" $ do
    unitBtn  <- divClass "unit-button" $ button "Get unit"
    intBtn   <- divClass "int-button"  $ button "Get int"

    unitResponse <- getUnit unitBtn
    intResponse :: Event t (ReqResult Int) <- getInt intBtn

    score <- foldDyn (+) 0 (fmapMaybe reqSuccess intResponse)

    r <- holdDyn "Waiting" $ fmap showXhrResponse $
         leftmost [fmapMaybe response unitResponse
                  ,fmapMaybe response intResponse
                  ]
    divClass "unit-int-response" $ el "p" $ dynText r >> el "br" (return ()) >> text "Total: " >> display score

  elClass "div" "demo-group" $ do

    text "Name"
    el "br" $ return ()
    inp :: Dynamic t Text <- fmap value $ divClass "name-input" $ (textInput def)
    let checkedName = fmap (\i -> bool (QParamSome i) (QParamInvalid "Need a name") (T.null i)) inp
    el "br" $ return ()

    text "Greetings (space-separated)"
    el "br" $ return ()
    greetings <- fmap (fmap T.words . value) $
      divClass "greetings-input" $ (textInput def)

    el "br" $ return ()

    gusto <- fmap value $ divClass "gusto-input" $ checkbox False def

    el "br" $ return ()
    sayhiClicks :: Event t () <- divClass "hi-button" $ button "Say hi"
    let triggers = leftmost [sayhiClicks, () <$ updated inp]

    resp <- sayhi checkedName greetings gusto triggers
    divClass "greeting-response" $ dynText =<<
      holdDyn "No hi yet" (leftmost [ fmapMaybe reqSuccess resp
                                    , fmapMaybe reqFailure resp])

  elClass "div" "demo-group" $ do
    text "A Double to double"
    el "br" $ return ()
    dblinp  <- fmap value $ divClass "double-input" $ textInput def
    dblBtn  <- divClass "double-button" $ button "Double it"
    dblResp <- dbl (fmap (note "read failure" . readMaybe . T.unpack) $
                          dblinp) dblBtn
    divClass "double-errors" $ dynText =<<
      holdDyn "(no errors)" (fmapMaybe reqFailure dblResp)
    el "br" (return ())
    divClass "double-result" $ el "p" $ dynText =<<
      holdDyn "No number yet" (fmap tShow $
                               fmapMaybe reqSuccess dblResp)

  elClass "div" "demo-group" $ do
    text "Multi-part path"
    b <- value <$> checkbox False def
    mpGo <- button "Test"
    multiResp <- multi b mpGo
    dynText =<< holdDyn "No res yet" (fmap tShow $
                                      fmapMaybe reqSuccess $
                                      multiResp)

  el "br" $ return ()

  elClass "div" "demo-group" $ do
    text "JSON Unicode encoding test"
    txt <- value <$> textInput def
    ev  <- button "Question"
    let dQ = Right . Question <$> traceDyn "will send: " txt
    rr  <- qna dQ ev
    el "p" $
      dynText =<< holdDyn "No Answer" (unAnswer <$> fmapMaybe reqSuccess rr)

showXhrResponse :: XhrResponse -> Text
showXhrResponse (XhrResponse stat stattxt rbmay rtmay respHeaders) =
  T.unlines ["stat: " <> tShow stat
            ,"stattxt: " <> tShow stattxt
            ,"resp: " <> maybe "" showRB rbmay
            ,"rtext: " <> tShow rtmay
            ,"rHeaders: " <> tShow respHeaders]

tShow :: Show a => a -> Text
tShow = T.pack . show

showRB :: XhrResponseBody -> Text
showRB (XhrResponseBody_Default t) = tShow t
showRB (XhrResponseBody_Text t) = tShow t
showRB (XhrResponseBody_Blob t) = "<Blob>"
showRB (XhrResponseBody_ArrayBuffer t) = tShow t

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
