{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module HummingBird.Upstream where

import Control.Lens (makeClassyPrisms, makeClassy, view)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)

import           Data.IORef (IORef, readIORef)
import qualified Network.DNS as DNS

data UpstreamError
    = UpstreamResolverError String
    | UpstreamSocketError String
    deriving (Show, Eq)
makeClassyPrisms ''UpstreamError

newtype UpstreamEnv = UpstreamEnv
    { _upstreamResolvers    :: IORef [DNS.Resolver]
    }
makeClassy ''UpstreamEnv

type UpstreamProvision c e m = 
    ( MonadIO m
    , MonadReader c m, HasUpstreamEnv c
    , MonadError e m, AsUpstreamError e
    )

resolve :: UpstreamProvision c e m => DNS.Question -> m (Either DNS.DNSError [DNS.ResourceRecord])
resolve (DNS.Question qd qt) = do
    ior <- view upstreamResolvers
    rs  <- liftIO $ readIORef ior
    rv  <- liftIO $ DNS.lookup (head rs) qd qt
    case rv of
        Left e      -> pure (Left e)
        Right rr    -> pure (Right $ map wrapper rr)

    where
        wrapper :: DNS.RData -> DNS.ResourceRecord
        wrapper rdata = DNS.ResourceRecord qd (getType rdata) DNS.classIN 30 rdata

        getType :: DNS.RData -> DNS.TYPE
        getType DNS.RD_A{}     = DNS.A
        getType DNS.RD_NS{}    = DNS.NS
        getType DNS.RD_CNAME{} = DNS.CNAME
        getType DNS.RD_SOA{}   = DNS.SOA
        getType DNS.RD_NULL{}  = DNS.NULL
        getType DNS.RD_PTR{}   = DNS.PTR
        getType DNS.RD_MX{}    = DNS.MX
        getType DNS.RD_TXT{}   = DNS.TXT
        getType DNS.RD_AAAA{}  = DNS.AAAA
        getType _              = qt