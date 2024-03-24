{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module HummingBird.Downstream (Downstream (..)) where

import Control.Concurrent.STM (TChan)
import Control.Exception (Exception)

import HummingBird.Event (Event)

class (Monad m, Exception (DownstreamException m)) => Downstream m where
    type DownstreamException m

    listen :: TChan Event -> m (Either (DownstreamException m) ())
