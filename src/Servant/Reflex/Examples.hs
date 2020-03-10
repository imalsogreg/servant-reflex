-- |

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


module Servant.Reflex.Examples where

import Control.Monad.Identity (Identity(..)) -- Temporary
import           Data.Proxy                    (Proxy (..))
import           Reflex                        (Event, MonadHold, PostBuild,
                                                holdDyn, leftmost)
import           Reflex.Dom.Core               (DomBuilder, display)
import           Reflex.Dom.Widget             (button)
import           Servant.API                   ((:<|>) (..), (:>) (..))
import qualified Servant.API                   as API
import           Servant.Common.BaseUrl        (SupportsServantReflex)

import qualified Servant.Reflex.TupleRequest   as TupleRequest
import qualified Servant.Reflex.TupleRequestMulti   as TupleRequestMulti
import qualified Servant.Test.ComprehensiveAPI as Servant


type MyAPI =

  API.Header "hi" Int
    :> API.Capture "hello" Int
    :> API.Capture "hello2" Bool
    :> API.Get '[API.JSON] (API.Headers '[API.Header "hi" Int] Int)

  :<|> API.Get '[API.JSON] Bool

type MyAPI2 = API.Raw


m :: forall t m.(SupportsServantReflex t m, MonadHold t m, PostBuild t m, DomBuilder t m) => m ()
m = do
  let (a :<|> b) = TupleRequest.clientR (Proxy @MyAPI) (Proxy :: Proxy m) (Proxy :: Proxy t) undefined
  btn <- button "Hello"
  resA <- a ((2, 5, True) <$ btn)
  resB <- b (() <$ btn)
  display =<< holdDyn "waiting" (leftmost [show <$> resB ])

  let rawR = TupleRequest.clientR (Proxy @MyAPI2) (Proxy @m) (Proxy @t) undefined

  let (aMulti1 :<|> bMulti1) =
        TupleRequestMulti.clientR
        (Proxy @MyAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy t)
        (Proxy :: Proxy ((,) Int))
        undefined
  return ()

