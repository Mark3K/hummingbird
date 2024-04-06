{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module HummingBird.Processor where

import Control.Lens (makeClassyPrisms, makeClassy, view)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)

data ProcessorError
    = ProcessorXError Int
    | ProcessorYError Int
    deriving (Show, Eq)
makeClassyPrisms ''ProcessorError

data ProcessorEnv = ProcessorEnv
    { _processorEnvX    :: Int
    , _processorEnvY    :: Int
    } deriving (Show, Eq)
makeClassy ''ProcessorEnv

type ProcessorProvision c e m =
    ( MonadIO m
    , MonadReader c m, HasProcessorEnv c
    , MonadError e m, AsProcessorError e
    )

run :: ProcessorProvision c e m => m ()
run = do
    x <- view processorEnvX
    y <- view processorEnvY
    -- TODO
    pure ()
