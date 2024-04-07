{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens ((.~), (^.))
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.Logger (LogLevel(..), LoggingT (runLoggingT), defaultOutput)

import Data.Maybe (fromMaybe)
import Data.IP (IP)

import Network.Socket (PortNumber)

import Options.Applicative
import System.IO (withFile, IOMode (AppendMode), hSetBuffering, BufferMode (LineBuffering), stdout)
import Text.Read (readEither)

import HummingBird

data Params = Params
    { configPath    :: String
    , logFile       :: String
    , upstreams     :: [(IP, Maybe PortNumber)]
    , verbose       :: Int
    , version       :: Bool
    } deriving (Show)

params :: Parser Params
params = Params 
    <$> strOption (long "config-path" <> metavar "<PATH>" <> help "The configuration file path" <> showDefault <> value "")
    <*> strOption (long "log-file" <> metavar "<PATH>" <> help "The log file path" <> showDefault <> value "")
    <*> upstreamsParser
    <*> (length <$> many (flag' () (short 'v' <> help "verborsity")))
    <*> switch (long "version" <> help "Show the program verion")

upstreamsParser :: Parser [(IP, Maybe PortNumber)]
upstreamsParser = many $ upstreamOption 
    (  short 'u' 
    <> long "up-stream" 
    <> metavar "<UPSTREAM>" 
    <> help "An upstream to be used (can be specified multiple times)"
    )


upstreamOption :: Mod OptionFields (IP, Maybe PortNumber) -> Parser (IP, Maybe PortNumber)
upstreamOption = option $ eitherReader $ \s -> case split ':' s of
    [x]     -> case readEither x of
        Left  _ -> Left "Invalid IP Address"
        Right v -> pure (v, Nothing)

    [x,y]   -> case readEither x of
        Left   _ -> Left "Invalid IP Address"
        Right v0 -> case readEither y of
            Left   _ -> Left "Invalid Port Number"
            Right v1 -> pure (v0, v1)

    _       -> Left "Upstream should be (ip | ip:port)"

    where
        split :: Char -> String -> [String]
        split sep xs = case break (==sep) xs of
            (s, "")     -> [s]
            (s, _:rs)   -> s : split sep rs

verboseToLogLevel :: Int -> Maybe LogLevel
verboseToLogLevel 0 = Just LevelError
verboseToLogLevel 1 = Just LevelWarn
verboseToLogLevel 2 = Just LevelInfo
verboseToLogLevel 3 = Just LevelDebug
verboseToLogLevel _ = Nothing

buildConfig :: Params -> IO Config
buildConfig Params{..} = do
    cfg <- loadConfig configPath
    pure $ cfgLogLevel  .~ logLevel cfg
         $ cfgLogOutput .~ logOutput 
         $ cfgUpstreams .~ upstreams
         $ cfg
    where
        logLevel cfg    = fromMaybe (cfg ^. cfgLogLevel) (verboseToLogLevel verbose)
        logOutput       = if logFile /= "" then FileOutput logFile else Stdout

loadConfig :: FilePath -> IO Config
loadConfig path = pure defaultConfig

run :: Params -> IO ()
run vars = do
    rv <- run' vars
    case rv of
        Left e  -> putStrLn ("AppError: " <> show e)
        Right _ -> pure ()

run' :: Params -> IO (Either AppError ())
run' vars = do
    cfg <- buildConfig vars
    env <- (appEnvConfig .~ cfg) <$> defaultAppEnv
    case env ^. appEnvConfig . cfgLogOutput of
        FileOutput fp -> withFile fp AppendMode $ \h -> 
            hSetBuffering h LineBuffering >> runLoggingT (runApp env app) (defaultOutput h)
        Stdout        -> runLoggingT (runApp env app) (defaultOutput stdout)

    where
        runApp  :: AppEnv 
                -> ExceptT AppError (ReaderT AppEnv (LoggingT IO)) a 
                -> LoggingT IO (Either AppError a)
        runApp env = flip runReaderT env . runExceptT

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (params <**> helper)
                    (  fullDesc 
                    <> progDesc "HummingBird" 
                    <> header "HummingBird - A simple dns proxy implemented in Haskell"
                    )