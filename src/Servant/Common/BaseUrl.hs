{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
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

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.List
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics
import           Network.URI hiding (path)
import           Reflex
import           Reflex.Dom
import           Safe
import           Text.Read

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Read, Eq, Ord, Generic)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseFullUrl Scheme String Int String
             | BasePath String
  deriving (Ord, Read, Show, Generic)


instance Eq BaseUrl where
    BasePath s == BasePath s' = s == s'
    BaseFullUrl a b c path == BaseFullUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s ('/':x) = x
              s x       = x
    _ == _ = False

showBaseUrl :: BaseUrl -> String
showBaseUrl (BasePath s) = s
showBaseUrl (BaseFullUrl urlscheme host port path) =
  schemeString ++ "//" ++ host ++ (portString </> path)
    where
      a </> b = if "/" `isPrefixOf` b || null b then a ++ b else a ++ '/':b
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" ++ show port

baseUrlWidget :: forall t m .MonadWidget t m => m (Dynamic t BaseUrl)
baseUrlWidget = elClass "div" "base-url" $ do
  urlWidget <- dropdown (0 :: Int) (constDyn $ 0 =: "BaseUrlFull" <> 1 =: "BasePath") def
  bUrlWidget <- forDyn (value urlWidget) $ \i -> case i of
    0 -> fullUrlWidget
    1 -> pathWidget
    _ -> error "Surprising value"
  joinDyn <$> widgetHold fullUrlWidget (updated bUrlWidget)
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
          port :: Dynamic t Int <- holdDyn 80 (fmapMaybe readMaybe $ updated (value prt))
          path <- textInput def { _textInputConfig_attributes = constDyn $ "placeholder" =: "a/b" }
          BaseFullUrl `mapDyn` value schm `apDyn` value srv `apDyn` port `apDyn` value path

apDyn :: MonadWidget t m => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
apDyn f' a = do
  f <- f'
  combineDyn ($) f a
