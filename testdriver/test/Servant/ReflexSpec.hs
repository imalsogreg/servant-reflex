{-# language OverloadedStrings #-}

module Servant.ReflexSpec where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Foldable
import Test.WebDriver.Commands.Wait
import Test.Hspec.WebDriver
import Test.Hspec.WebDriver (chromeCaps)


clickingShouldCause :: Selector -> Selector -> Double -> (Element -> Element -> WD Bool) -> WD ()
clickingShouldCause inp outp timeLimit s = do
  inputEl  <- findElem inp
  outputEl <- findElem outp
  click inputEl
  _ <- waitUntil timeLimit $ do
    r <- s inputEl outputEl
    return r
  return ()

hasInfixText :: Element -> T.Text -> WD Bool
hasInfixText el t = getText el >>= \txt -> return $ t `T.isInfixOf` txt

spec :: Spec
spec = do
  describe "servant-reflex tests" $ do

    session "test page" $ using [chromeCaps] $ do

      it "opens the page" $ runWD $
        openPage "http://localhost:8000"

      it "has a div" $ runWD $ do
        e <- waitUntil 5 $ findElem $ ByCSS "div"
        e `shouldBeTag` "div"

      it "has demo-group div" $ runWD $ do
        e <- findElem $ ByCSS "div.demo-group"
        e `shouldBeTag` "div"

      it "doesn't have demo-group-nonexistent div" $ runWD $ do
        e <- findElems $ ByCSS "div.demo-group.nonexistent"
        e `shouldBe` []

      it "can get an int or unit from the API" $ runWD $ do
        let outEl = ByCSS "div.unit-int-response p"
        clickingShouldCause (ByCSS "div.int-button > button") outEl 0.1
          (\i o -> o `hasInfixText` "Just \"100\"")
        clickingShouldCause (ByCSS "div.unit-button > button") outEl 0.1
          (\i o -> o `hasInfixText` "Just \"[[]]\"")

      let setupGreeting = do
            iName       <- findElem $ ByCSS ".name-input input"
            iGreetings  <- findElem $ ByCSS ".greetings-input input"
            bGusto      <- findElem $ ByCSS ".gusto-input input"
            goBtn       <- findElem $ ByCSS ".hi-button button"
            greetingRes <- findElem $ ByCSS ".greeting-response"
            return (iName, iGreetings, bGusto, goBtn, greetingRes)

      it "handle query params" $ runWD $ do
        (iName, iGreetings, bGusto, goBtn, greetingRes) <- setupGreeting
        sendKeys "Haskell" iName
        sendKeys "Hello Hi" iGreetings
        click goBtn
        greetingRes `shouldHaveText` "Hello, and Hi, Haskell"

      it "handles queryflag" $ runWD $ do
        (iName, iGreetings, bGusto, goBtn, greetingRes) <- setupGreeting
        click bGusto
        click goBtn
        greetingRes `shouldHaveText` "HELLO, AND HI, HASKELL"

      it "tags dyn promptly" $ runWD $ do
        (iName, iGreetings, bGusto, goBtn, greetingRes) <- setupGreeting
        sendKeys "!" iName
        greetingRes `shouldHaveText` "HELLO, AND HI, HASKELL!"

{- Broken. Why?
      it "handles request bodies" $ runWD $ do
        dText <- findElem $ ByCSS ".double-input input"
        dSend <- findElem $ ByCSS ".double-button button"
        dRes  <- findElem $ ByCSS ".double-result p"
        dErr  <- findElem $ ByCSS ".double-errors"
        sendKeys "200" dText
        click dSend
        saveScreenshot "/home/ubuntu/screen.png"
        liftIO (threadDelay 1000000)
        dErr `shouldHaveText` "(no errors)"
        dRes `shouldHaveText` "400.0"
-}
