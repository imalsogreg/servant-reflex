{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Servant.Client.Internal.ReflexClient where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT (..), runExceptT )
import Control.Monad.Reader(MonadReader, ReaderT (..))
import Data.Functor.Alt (Alt(..))
-- import Control.Monad.Trans.Control
import GHC.Generics
import Servant.Client.Core
import Reflex


data ClientEnv = ClientEnv
  { baseUrl :: BaseUrl }

newtype ClientM t a = ClientM { unClientM :: ReaderT ClientEnv (ExceptT ServantError IO) (Event t a) }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ServantError, MonadThrow
           , MonadCatch )

-- instance MonadBase IO ClientM where
--   liftBase = ClientM . liftBase

-- instance MonadBaseControl IO ClientM where
--   type StM ClientM a = Either ServantError a
--   liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . unClientM)))
--   restoreM st = ClientM (restoreM st)

instance Alt (ClientM t) where
  a <!> b = a `catchError` \_ -> b

instance ClientLike (ClientM t a) (ClientM t a) where
  mkClient = id

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ flip runReaderT env $ unClientM cm

