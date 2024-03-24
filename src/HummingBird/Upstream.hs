{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module HummingBird.Upstream ( Upstream (..)) where

import Control.Exception (Exception)

import Network.DNS (DNSMessage)

class (Monad m, Exception (UpstreamException m)) => Upstream m where
    type UpstreamException m
    proxy :: DNSMessage -> m (Either (UpstreamException m) DNSMessage)
