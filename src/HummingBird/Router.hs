module HummingBird.Router (Router, new, insert, get, find, size) where

import qualified Data.ByteString.Char8 as BS
import Data.Trie (Trie)
import qualified Data.Trie as Trie

import Network.DNS (Domain, Resolver, normalize)

newtype Router = Router (Trie [Resolver])

new :: Router
new = Router Trie.empty

insert :: Router -> Domain -> Resolver -> Router
insert (Router rr) dn r
    | Trie.member key rr = Router $ Trie.adjust (r:) key rr
    | otherwise          = Router $ Trie.insert key [r] rr
    where key = normalizeDomain dn

normalizeDomain :: Domain -> Domain
normalizeDomain dn
    | BS.null dn = BS.singleton '.'
    | otherwise  = BS.reverse (BS.cons '.' $ normalize dn)

get :: Router -> Domain -> Maybe [Resolver]
get (Router rr) dn = Trie.lookup (normalizeDomain dn) rr

find :: Router -> Domain -> Maybe [Resolver]
find (Router rr) dn = case Trie.match rr (normalizeDomain dn) of
    Nothing          -> Nothing
    Just (_, res, _) -> Just res

size :: Router -> Int
size (Router rr) = Trie.size rr