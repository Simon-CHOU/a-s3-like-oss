{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point: CLI argument parsing and server startup.
module Main (main) where

import RIO
import S3OSS.Config
import S3OSS.Server
import Options.Applicative

-- | CLI options.
data CliOptions = CliOptions
  { optConfig   :: Maybe FilePath
  , optPort     :: Maybe Int
  , optDataDir  :: Maybe FilePath
  , optTlsCert  :: Maybe FilePath
  , optTlsKey   :: Maybe FilePath
  , optDevMode  :: Bool
  }

cliParser :: Parser CliOptions
cliParser = CliOptions
  <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Config file path (YAML)"))
  <*> optional (option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Listen port"))
  <*> optional (strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> help "Data directory"))
  <*> optional (strOption (long "tls-cert" <> metavar "FILE" <> help "TLS certificate file"))
  <*> optional (strOption (long "tls-key" <> metavar "FILE" <> help "TLS private key file"))
  <*> switch (long "dev" <> help "Development mode (disable TLS and auth)")

main :: IO ()
main = do
  opts <- execParser $ info (cliParser <**> helper)
    (fullDesc <> progDesc "s3-oss: Secure S3-compatible local object storage" <> header "s3-oss")

  -- Load config file or use defaults
  baseConfig <- case optConfig opts of
    Just path -> loadConfig path
    Nothing   -> pure defaultConfig

  -- Apply CLI overrides
  let config = applyOverrides baseConfig opts

  -- Start server
  startServer config

-- | Apply CLI option overrides to resolved config.
applyOverrides :: ResolvedConfig -> CliOptions -> ResolvedConfig
applyOverrides cfg opts = cfg
  { rcServer = (rcServer cfg)
    { scPort = fromMaybe (scPort $ rcServer cfg) (optPort opts)
    , scTlsCert = optTlsCert opts <|> scTlsCert (rcServer cfg)
    , scTlsKey  = optTlsKey opts  <|> scTlsKey (rcServer cfg)
    , scDevelopmentMode = optDevMode opts || scDevelopmentMode (rcServer cfg)
    }
  , rcStorage = (rcStorage cfg)
    { stDataDir = fromMaybe (stDataDir $ rcStorage cfg) (optDataDir opts)
    }
  }
