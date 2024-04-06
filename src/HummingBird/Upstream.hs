{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module HummingBird.Upstream where
import Control.Lens (makeClassyPrisms, makeClassy)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)

data UpstreamError
    = UpstreamAError String
    | UpstreamBError String
    deriving (Show, Eq)
makeClassyPrisms ''UpstreamError

data UpstreamEnv = UpstreamEnv
    { _upstreamEnvA     :: Int
    , _upstreamEnvB     :: Int
    } deriving(Show, Eq)
makeClassy ''UpstreamEnv

type UpstreamProvision c e m = 
    ( MonadIO m
    , MonadReader c m, HasUpstreamEnv c
    , MonadError e m, AsUpstreamError e
    )

run :: UpstreamProvision c e m => m ()
run = undefined