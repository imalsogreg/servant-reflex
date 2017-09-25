{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import Data.Bool
import Data.Maybe
import Control.Monad.Fix (MonadFix)
import Data.Monoid (First(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits
import Servant.API
import API
import Data.Proxy
import Text.Read (readMaybe)
import Reflex.Dom hiding (run)
------------------------------------------------------------------------------
import Servant.Reflex
import Servant.Reflex.Multi


api :: Proxy API
api = Proxy

main :: IO ()
main = mainWidget $ do
    divClass "example-base" run
    divClass "example-multi" runMulti


runMulti :: forall t m. (SupportsServantReflex t m,
                        DomBuilder t m,
                        DomBuilderSpace m ~ GhcjsDomSpace,
                        MonadFix m,
                        PostBuild t m,
                        MonadHold t m) => m ()
runMulti = do
    url <- baseUrlWidget
    el "br" blank
    let (_ :<|> _ :<|> sayHi :<|> dbl :<|> _ :<|> _ :<|> _ :<|> _ ) =
            clientA api (Proxy :: Proxy m) (Proxy :: Proxy []) (Proxy :: Proxy Int) url

    num :: Dynamic t (Either Text Double) <- fmap (note "No read" . readMaybe . T.unpack) . value <$> textInput def
    num2 :: Dynamic t (Either Text Double) <-  fmap (note "No read" . readMaybe . T.unpack) . value <$> textInput def

    b <- button "Run dbl multi"
    reqCount :: Dynamic t Int <- count b
    r <- dbl ((\x y -> [x,y]) <$> num <*> num2) (tag (current reqCount) b)
    dynText =<< holdDyn "Waiting" (T.pack . show .  fmap reqSuccess <$> r)

    lastInd <- holdDyn [] $ fmap reqTag <$> r
    display lastInd

    divClass "demo-group" $ do
        nms <- fmap (fmap T.words . value) $ divClass "" $ do
            text "Names"
            textInput def
        grts <- fmap (fmap T.words . value) $ divClass "" $ do
            text "Greetings"
            textInput def
        gust <- fmap (value) $ divClass "gusto-input" $ checkbox False def
        b <- button "Go"
        r' <- sayHi (fmap QParamSome <$> nms) (fmap (:[]) $ grts)
                    (constDyn [True, False]) (1 <$ b)

        dynText =<< holdDyn "Waiting" (T.pack . show . catMaybes .
                                       fmap reqSuccess  <$> r')


    return ()

run :: forall t m. (SupportsServantReflex t m,
                    DomBuilder t m,
                    DomBuilderSpace m ~ GhcjsDomSpace,
                    MonadFix m,
                    PostBuild t m,
                    MonadHold t m) => m ()
run = mdo

  reqCount <- count $ leftmost
              [() <$ unitBtn, () <$ intBtn, () <$ sayHiClicks, () <$ dblBtn, () <$ mpGo]
  -- Allow user to choose the url target for the request
  -- (alternatively we could just `let url = constDyn (BasePath "/")`)
  url <- baseUrlWidget
  el "br" (return ())
  dynText $ showBaseUrl <$> url

  el "br" (return ())

  -- Name the computed API client functions
  let tweakRequest = ClientOptions $ \r -> do
          putStrLn ("Got req: " ++ show r)
          return $ r & withCredentials .~ True
  let (getUnit :<|> getInt :<|> sayhi :<|> dbl
       :<|> multi :<|> qna :<|> secret :<|> doRaw) =
        clientWithOpts api (Proxy :: Proxy m) (Proxy :: Proxy Int) url tweakRequest

      c2 = client (Proxy :: Proxy ComprehensiveAPI) (Proxy :: Proxy m) (Proxy :: Proxy ()) url -- Just make sure this compiles for now

  (unitBtn, intBtn) <- elClass "div" "demo-group" $ do
    unitBtn  <- divClass "unit-button" $ button "Get unit"
    intBtn   <- divClass "int-button"  $ button "Get int"

    unitResponse <- getUnit $ tag (current reqCount) unitBtn
    intResponse :: Event t (ReqResult Int Int) <- getInt $ tag (current reqCount) intBtn

    score <- foldDyn (+) 0 (fmapMaybe reqSuccess (intResponse))

    r <- holdDyn "Waiting" $ fmap showXhrResponse $
         leftmost [fmapMaybe response (unitResponse)
                  ,fmapMaybe response (intResponse)
                  ]
    divClass "unit-int-response" $ el "p" $ dynText r >> el "br" (return ()) >> text "Total: " >> display score
    return (unitBtn, intBtn)

  sayHiClicks <- elClass "div" "demo-group" $ do

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
    sayHiClicks :: Event t () <- divClass "hi-button" $ button "Say hi"
    let triggers = leftmost [sayHiClicks, () <$ updated inp]

    resp <- sayhi checkedName greetings gusto (tag (current reqCount) triggers)
    divClass "greeting-response" $ dynText =<<
      holdDyn "No hi yet" (leftmost [ fmapMaybe reqSuccess (resp)
                                    , fmapMaybe reqFailure (resp)])
    return sayHiClicks

  dblBtn <- elClass "div" "demo-group" $ do
    text "A Double to double"
    el "br" $ return ()
    dblinp <- fmap value $ divClass "double-input" $ textInput def
    (dblBtn) <- divClass "double-button" $ button "Double it"
    dblResp <- dbl (fmap (note "read failure" . readMaybe . T.unpack) $
                          dblinp) (tag (current reqCount) dblBtn)
    divClass "double-errors" $ dynText =<<
      holdDyn "(no errors)" (fmapMaybe reqFailure (dblResp))
    el "br" (return ())
    divClass "double-result" $ el "p" $ dynText =<<
      holdDyn "No number yet" (fmap tShow $
                               fmapMaybe reqSuccess (dblResp))
    return dblBtn

  mpGo <- elClass "div" "demo-group" $ do
    text "Multi-part path"
    b <- value <$> checkbox False def
    mpGo <- button "Test"
    multiResp <- multi b (tag (current reqCount) mpGo)
    dynText =<< holdDyn "No res yet" (fmap tShow $
                                      fmapMaybe reqSuccess $
                                      (multiResp))
    return mpGo

  return ()

  el "br" $ return ()

  elClass "div" "demo-group" $ do
    text "JSON Unicode encoding test"
    txt <- value <$> textInput def
    ev  <- fmap (1 <$) $ button "Question"
    let dQ = Right . Question <$> traceDyn "will send: " txt
    rr  <- qna dQ ev
    el "p" $
      dynText =<< holdDyn "No Answer" (unAnswer <$> fmapMaybe reqSuccess rr)

  divClass "demo-group" $ do
    un <- fmap value $ text "Username"    >> textArea def
    pw <- fmap value $ text "Unhidden PW" >> textArea def
    let ba :: Dynamic t (BasicAuthData ) = BasicAuthData
                                           <$> fmap T.encodeUtf8 un
                                           <*> fmap T.encodeUtf8 pw
    b <- button "Get secret"
    r  <- secret (Just <$> ba) (0 <$ b)
    res <- holdDyn Nothing (reqSuccess <$> r)
    display res


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
