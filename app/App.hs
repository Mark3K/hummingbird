{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE UndecidableInstances           #-}

module App where

import Control.Lens (view, over)
import Control.Monad.Base (MonadBase (liftBase), liftBaseDefault)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (local), ReaderT (runReaderT), MonadTrans(lift))
import Control.Monad.Trans.Control
    ( defaultLiftBaseWith
    , defaultLiftWith
    , defaultRestoreM
    , defaultRestoreT
    , ComposeSt
    , MonadTransControl(..)
    , MonadBaseControl(..)
    )

import Katip (Katip(..), KatipContext(..))

import HummingBird

newtype AppT m a = AppT
    { runAppT :: ReaderT AppEnv m a 
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader AppEnv)

runApp :: AppEnv -> AppT m a -> m a 
runApp env m = runReaderT (runAppT m) env

instance MonadTrans AppT where
    lift = AppT . lift

instance MonadBase b m => MonadBase b (AppT m) where
    liftBase = liftBaseDefault

instance MonadTransControl AppT where
  type StT AppT a = StT (ReaderT AppEnv) a
  liftWith = defaultLiftWith AppT runAppT

  restoreT = defaultRestoreT AppT

instance MonadBaseControl b m => MonadBaseControl b (AppT m) where
  type StM (AppT m) a = ComposeSt AppT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (MonadIO m) => Katip (AppT m) where
    getLogEnv = view (appEnvLogger . loggerEnv)

    localLogEnv f (AppT m) = AppT $ local (over (appEnvLogger . loggerEnv) f) m

instance (MonadIO m) => KatipContext (AppT m) where
    getKatipContext = view (appEnvLogger . loggerContexts)

    localKatipContext f (AppT m) = AppT $ local (over (appEnvLogger . loggerContexts) f) m

    getKatipNamespace = view (appEnvLogger . loggerNamespace)

    localKatipNamespace f (AppT m) = AppT $ local (over (appEnvLogger . loggerNamespace) f) m