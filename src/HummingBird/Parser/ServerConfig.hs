module HummingBird.Parser.ServerConfig (routesFromFile) where

import Data.ByteString.Char8 (pack)
import Data.IP (IP (IPv4), IPv6, IPv4, toIPv4)
import Data.Maybe (catMaybes)

import Network.DNS (Domain)
import Network.Socket (PortNumber)
import Text.ParserCombinators.Parsec
    ( Parser
    , char
    , optionMaybe
    , many
    , octDigit
    , string
    , try
    , oneOf
    , parse
    , eof
    , (<|>)
    , (<?>), anyChar
    )

import HummingBird.Types (Route (..), Upstream (..))

data RoutePart 
    = RouteDomain   Domain 
    | RouteUpstream (IP, Maybe PortNumber)
    deriving (Show, Eq)

routesFromFile :: FilePath -> IO (Either String [Route])
routesFromFile path = do
    raw <- readFile path
    case parse routes path raw of
        Left   e -> pure $ Left (show e)
        Right rs -> pure $ Right rs 

routes :: Parser [Route]
routes = do
    rs <- many maybeRoute
    eof
    pure $ catMaybes rs

maybeRoute :: Parser (Maybe Route)
maybeRoute = 
        do { _ <- comment; pure Nothing } 
    <|> do { _ <- blankLine; pure Nothing }
    <|> do Just <$> route 

blankLine :: Parser ()
blankLine = do
    _ <- eol
    pure ()

comment :: Parser String
comment = do
    _ <- char '#'
    s <- many anyChar
    _ <- optionMaybe eol
    pure s

route :: Parser Route
route = do 
    _     <- string "server=/"
    parts <- many routePart
    let doms = [dom | RouteDomain   dom <- parts]
        ipps = [is  | RouteUpstream  is <- parts]
    if length ipps /= 1
        then fail "More than one IP address found"
        else pure $ Route doms $ Upstream (ip ipps) (pt ipps)
    where 
        ip = fst . head
        pt = snd . head

routePart :: Parser RoutePart
routePart = try (RouteDomain <$> domain) <|> (RouteUpstream <$> upstream)

domain :: Parser Domain
domain = do
    chars <- many (oneOf (".-" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']))
    _     <- char '/'
    pure $ pack chars

upstream :: Parser (IP, Maybe PortNumber)
upstream = do
    v4 <- ipv4 
    _  <- optionMaybe (char '#')
    pn <- optionMaybe port
    _  <- optionMaybe eol
    pure (IPv4 v4, pn)

ipv4 :: Parser IPv4
ipv4 = do
    part0 <- many octDigit >>= toInt
    _     <- char '.'
    part1 <- many octDigit >>= toInt
    _     <- char '.'
    part2 <- many octDigit >>= toInt
    _     <- char '.'
    part3 <- many octDigit >>= toInt
    pure $ toIPv4 [part0, part1, part2, part3]
    where
        toInt s =
            if i > 255 
                then fail "All octets in an IPv4 address must be between 0 and 255"
                else pure i
            where i = toDecimal s

port :: Parser PortNumber
port = many octDigit >>= toPort
    where
        toPort s = 
            if i > 65535
                then fail "Port number must be between 0 and 65535"
                else pure i
            where i = toDecimal s

toDecimal :: Integral a => [Char] -> a
toDecimal = foldl step 0
    where 
        step a w = a * 10 + fromIntegral (fromEnum w - 48)

-- | TODO: to support IPv6
ipv6 :: Parser IPv6
ipv6 = undefined

-- eol :: Parser 
eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of Line"