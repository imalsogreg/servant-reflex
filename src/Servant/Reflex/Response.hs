{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Response handlers
module Servant.Reflex.Response
  ( reloadOnAPIError
  , RefreshSupport
  ) where

import           Control.Monad               (join, void)
import qualified Data.Text                   as Text
import           JSDOM                       (currentDocument)
import           JSDOM.Generated.Document    (getLocation)
import           JSDOM.Types                 (Location, liftDOM)
import           Language.Javascript.JSaddle (MonadJSM, ( # ))
import           Reflex
import           Reflex.Dom
import           Servant.Reflex

is4xx :: ReqResult () a -> Bool
is4xx = maybe False (\req -> (_xhrResponse_status req < fromInteger 500) && (_xhrResponse_status req >= 400)) . response

isDecodeError  :: ReqResult () a -> Bool -- TODO find a better way of detecting this, this seems dumb
isDecodeError  =
  maybe False (Text.isInfixOf "Error in $") . reqFailure

-- | Alias for constraints that can refresh
type RefreshSupport t m = (MonadJSM m, MonadHold t m, DomBuilder t m, SupportsServantReflex t m)

-- | Reloads page and clears the cache on any 4xx response or on any
--   decoding errors.
--   Most endpoints want to have this.
--   These errors mean the client code is probably outdated.
--   If so a clear cache refresh will fix the issue.
--   We no longer care about the JS growing stale, any 4xx response will just refresh it.
--   It also saves time in that we don't have to bother presenting good error messages for 4xx respones.
--   The tradeoff is that our api should only return a 4xx if it thinks the client is broken.
--   If developer wants to show a serious error message to the user he shouldn't rely on status codes
--   anyway because they're not type safe (in servant).
--   Status codes also don't provide good user experience,
--   *or* dev experience. A code is simply not human friendly.
reloadOnAPIError :: (MonadJSM m, RefreshSupport t m) => Event t (ReqResult () a) -> m (Event t (ReqResult () a))
reloadOnAPIError reqResult = do
  void $ widgetHold blank $ ffor reqResult $
    \req -> if is4xx req || isDecodeError req then clearCacheReload else pure ()
  pure reqResult

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Location.reload Mozilla Location.reload documentation>
reload' :: MonadJSM m => Location -> Bool -> m ()
reload' self bool = liftDOM $ void $ self # funName $ bool
  where
    funName :: String
    funName = "reload"

-- | Refresh and clear the browser cache, eg all assets will be reloaded.
--   this could be used for example once you received a client error,
--   it probably means you've outdated code.
clearCacheReload :: MonadJSM m => m ()
clearCacheReload = do
      mayWin <- currentDocument
      loc <- sequence $ getLocation <$> mayWin
      maybe (pure ()) (flip reload' True) $ join loc
