{-# language OverloadedStrings #-}
module Servant.ReflexSpec where

import Test.Hspec.WebDriver

main :: IO ()
main = hspec $
  describe "servant-reflex tests" $ do

    session "test page" $ using defaultConfig $ do
      it "opens the page" $ runWD $
        openPage "http://localhost:8000"
