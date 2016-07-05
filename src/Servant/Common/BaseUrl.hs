{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.BaseUrl (
  -- * types
    BaseUrl (..)
  , Scheme (..)
  -- * functions
  , baseUrlWidget
  , showBaseUrl
) where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Reflex
import           Reflex.Dom
import           Text.Read

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Read, Eq, Ord, Generic)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseFullUrl Scheme Text Int Text
             | BasePath Text
  deriving (Ord, Read, Show, Generic)


instance Eq BaseUrl where
    BasePath s == BasePath s' = s == s'
    BaseFullUrl a b c path == BaseFullUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s x = if T.isPrefixOf "/" x then T.tail x else x
    _ == _ = False

showBaseUrl :: BaseUrl -> Text
showBaseUrl (BasePath s) = s
showBaseUrl (BaseFullUrl urlscheme host port path) =
  schemeString <> "//" <> host <> (portString </> path)
    where
      a </> b = if "/" `T.isPrefixOf` b || T.null b then a <> b else a <> "/" <> b
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" <> T.pack (show port)

baseUrlWidget :: forall t m .MonadWidget t m => m (Dynamic t BaseUrl)
baseUrlWidget = elClass "div" "base-url" $ do
  urlWidget <- dropdown (0 :: Int) (constDyn $ 0 =: "BasePath" <> 1 =: "BaseUrlFull") def
  bUrlWidget <- forDyn (value urlWidget) $ \i -> case i of
    0 -> pathWidget
    1 -> fullUrlWidget
    _ -> error "Surprising value"
  joinDyn <$> widgetHold pathWidget (updated bUrlWidget)
  where pathWidget :: m (Dynamic t BaseUrl)
        pathWidget = do
          text "Url base path"
          t <- textInput (def {_textInputConfig_attributes =
                          constDyn ("placeholder" =: "/a/b")})
          mapDyn BasePath (value t)
        fullUrlWidget :: m (Dynamic t BaseUrl)
        fullUrlWidget = do
          schm <- dropdown Https (constDyn $ Https =: "https" <> Http =: "http") def
          srv  <- textInput def {_textInputConfig_attributes = constDyn $ "placeholder" =: "example.com"}
          text ":"
          prt  <- textInput def { _textInputConfig_attributes = constDyn $ "placeholder" =: "80"}
          port :: Dynamic t Int <- holdDyn 80 (fmapMaybe (readMaybe . T.unpack) $ updated (value prt))
          path <- textInput def { _textInputConfig_attributes = constDyn $ "placeholder" =: "a/b" }
          BaseFullUrl `mapDyn` value schm `myApDyn` value srv `myApDyn` port `myApDyn` value path

myApDyn :: MonadWidget t m => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
myApDyn f' a = do
  f <- f'
  combineDyn ($) f a
