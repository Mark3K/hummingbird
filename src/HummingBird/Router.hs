{-# LANGUAGE TupleSections #-}

module HummingBird.Router 
    ( Router
    , newRouter
    , insertRoute
    , getResolvers
    , findResolvers
    , domainNum
    , resolverNum
    ) where

import qualified Data.ByteString.Char8 as BS

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Trie (Trie)
import qualified Data.Trie as Trie

import Network.DNS (Domain, Resolver, normalize)

import HummingBird.Types

data Router = Router 
    { _routerDomains    :: Trie [Upstream]
    , _routerResolvers  :: Map Upstream Resolver
    }

newRouter :: Router
newRouter = Router Trie.empty mempty

insertRoute :: Router -> Route -> Resolver -> Router
insertRoute router (Route doms upstream) resolver = foldl (`insertDomain` upstream) router' doms
    where router' = insertResolver router upstream resolver

insertDomain :: Router -> Upstream -> Domain -> Router
insertDomain (Router domains rs) upstream dn
    | Trie.member key domains = Router (Trie.adjust (upstream:) key domains) rs
    | otherwise               = Router (Trie.insert key [upstream] domains) rs
    where key = normalizeDomain dn

insertResolver :: Router -> Upstream -> Resolver -> Router
insertResolver r@(Router domains resolvers) upstream resolver
    | Map.member upstream resolvers = r
    | otherwise = Router domains $ Map.insert upstream resolver resolvers

getResolvers :: Router -> Domain -> Maybe [(Upstream, Resolver)]
getResolvers (Router domains resolvers) dn = do
    upstreams <- Trie.lookup (normalizeDomain dn) domains
    mapM (\u -> (u,) <$> Map.lookup u resolvers) upstreams

findResolvers :: Router -> Domain -> Maybe [(Upstream, Resolver)]
findResolvers (Router domains resolvers) dn = do
    (_, upstreams, _) <- Trie.match domains (normalizeDomain dn)
    mapM (\u -> (u,) <$> Map.lookup u resolvers) upstreams

domainNum :: Router -> Int
domainNum (Router domains _) = Trie.size domains

resolverNum :: Router -> Int
resolverNum (Router _ resolvers) = Map.size resolvers

normalizeDomain :: Domain -> Domain
normalizeDomain dn
    | BS.null dn = BS.singleton '.'
    | otherwise  = BS.reverse (BS.cons '.' $ normalize dn)