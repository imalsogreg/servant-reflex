{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Response handlers
--   Usefull for 'clientWithOptsAndResultHandler'
module Servant.Reflex.ReloadOnError
  ( reloadOnAPIError
  , reloadOnResponse
  , RefreshSupport
  , isAuthErr
  , is4xxErr
  , isDecodeError
  , locationReload
  ) where

import           Control.Monad               (join, void)
import qualified Data.Text                   as Text
import           JSDOM                       (currentDocument)
import           JSDOM.Generated.Document    (getLocation)
import           JSDOM.Types                 (Location, liftDOM)
import           Language.Javascript.JSaddle (MonadJSM, ( # ))
import           Reflex
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Xhr
import           Servant.Reflex

-- | Alias for constraints that can refresh
type RefreshSupport t m
   = (MonadJSM m, MonadHold t m, DomBuilder t m, SupportsServantReflex t m)

is4xxErr :: ReqResult () a -> Bool
is4xxErr =
  maybe
    False
    (\req ->
       (_xhrResponse_status req < fromInteger 500) &&
       (_xhrResponse_status req >= 400)) .
  response

isAuthErr :: ReqResult () a -> Bool
isAuthErr =
  maybe
    False
    (\req ->
       let stat = _xhrResponse_status req
       in (stat == fromInteger 401) ||
           (stat == fromInteger 402) || (stat == fromInteger 407)) .
  response

isDecodeError :: ReqResult () a -> Bool -- TODO find a better way of detecting this, is there no typesafe decode error?
isDecodeError = maybe False (Text.isInfixOf "Error in $") . reqFailure

-- | Reloads page and clears the cache on any 4xx response which is not auth related
--   or on any decoding errors.
--   Most endpoints want to have this.
--   These errors mean the client code is probably outdated.
--   This assumes client errros aren't used in your app logic,
--   for more flexibility see 'reloadOnResponse'
reloadOnAPIError ::
     (MonadJSM m, RefreshSupport t m)
  => Event t (ReqResult () a)
  -> m (Event t (ReqResult () a))
reloadOnAPIError =
  reloadOnResponse
    (\req -> (is4xxErr req && (not $ isAuthErr req)) || isDecodeError req)

-- | Reload and clears the cache if the predicate returns true
reloadOnResponse ::
     (MonadJSM m, RefreshSupport t m)
  => (ReqResult () a -> Bool)
  -> Event t (ReqResult () a)
  -> m (Event t (ReqResult () a))
reloadOnResponse predic reqResult = do
  void $
    widgetHold blank $
    ffor reqResult $ \req ->
      if predic req
        then locationReload True
        else pure ()
  pure reqResult

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Location.reload Mozilla Location.reload documentation>
reload' :: MonadJSM m => Location -> Bool -> m ()
reload' self bool = liftDOM $ void $ self # funName $ bool
  where
    funName :: String
    funName = "reload"

-- | Refresh and clear the browser cache, eg all assets will be reloaded.
--   this could be used for example once you received a client error,
--   it probably means you've got outdated code.
locationReload :: MonadJSM m => Bool -> m ()
locationReload isClearReload = do
  mayWin <- currentDocument
  loc <- sequence $ getLocation <$> mayWin
  maybe (pure ()) (flip reload' isClearReload) $ join loc
