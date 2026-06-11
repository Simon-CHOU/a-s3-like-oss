{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration loading and parsing.
module S3OSS.Config (module S3OSS.Config) where

import S3OSS.Prelude
import S3OSS.Types
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.IO.Error (ioError, userError)

-- | Server configuration.
data ServerConfig = ServerConfig
  { scHost            :: Text
  , scPort            :: Int
  , scTlsCert         :: Maybe FilePath
  , scTlsKey          :: Maybe FilePath
  , scDevelopmentMode :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Storage configuration.
data StorageConfig = StorageConfig
  { stDataDir :: FilePath
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | User configuration (from YAML -- secret key in plaintext).
data UserConfig = UserConfig
  { ucName      :: Text
  , ucAccessKey :: Text
  , ucSecretKey :: Text
  , ucPolicies  :: [PolicyConfig]
  }
  deriving (Show, Eq, Generic, FromJSON)

data PolicyConfig = PolicyConfig
  { pcEffect    :: Text
  , pcActions   :: [Text]
  , pcResources :: [Text]
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Full application configuration (from YAML).
data AppConfig = AppConfig
  { acServer  :: ServerConfig
  , acStorage :: StorageConfig
  , acUsers   :: [UserConfig]
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Resolved application configuration.
data ResolvedConfig = ResolvedConfig
  { rcServer  :: ServerConfig
  , rcStorage :: StorageConfig
  , rcUsers   :: [User]
  }

-- | Parse action string to Action type.
parseAction :: Text -> Either Text Action
parseAction "s3:GetObject"               = Right S3GetObject
parseAction "s3:PutObject"               = Right S3PutObject
parseAction "s3:DeleteObject"            = Right S3DeleteObject
parseAction "s3:HeadObject"              = Right S3HeadObject
parseAction "s3:CopyObject"              = Right S3CopyObject
parseAction "s3:ListObjects"             = Right S3ListObjects
parseAction "s3:CreateBucket"            = Right S3CreateBucket
parseAction "s3:DeleteBucket"            = Right S3DeleteBucket
parseAction "s3:ListBuckets"             = Right S3ListBuckets
parseAction "s3:HeadBucket"              = Right S3HeadBucket
parseAction "s3:CreateMultipartUpload"   = Right S3CreateMultipartUpload
parseAction "s3:UploadPart"              = Right S3UploadPart
parseAction "s3:CompleteMultipartUpload" = Right S3CompleteMultipartUpload
parseAction "s3:AbortMultipartUpload"    = Right S3AbortMultipartUpload
parseAction "s3:*"                       = Right S3AllActions
parseAction "*"                          = Right S3AllActions
parseAction x                            = Left $ "Unknown action: " <> x

-- | Parse 'PolicyConfig' to 'Policy'.
resolvePolicy :: PolicyConfig -> Either Text Policy
resolvePolicy pc = do
  effect <- case pcEffect pc of
    "Allow" -> Right Allow
    "Deny"  -> Right Deny
    x       -> Left $ "Unknown effect: " <> x
  actions <- traverse parseAction (pcActions pc)
  let resources = map ResourceARN (pcResources pc)
  pure $ Policy effect actions resources

-- | Resolve config: parse policies, store secret keys.
-- Returns 'Left' with an error message if any user policy fails to parse,
-- making the function total rather than calling 'error' on failure.
resolveConfig :: AppConfig -> Either Text ResolvedConfig
resolveConfig cfg = do
  users <- traverse resolveUser (acUsers cfg)
  pure $ ResolvedConfig
    { rcServer  = acServer cfg
    , rcStorage = acStorage cfg
    , rcUsers   = users
    }
  where
    resolveUser :: UserConfig -> Either Text User
    resolveUser uc = do
      policies <- traverse resolvePolicy (ucPolicies uc)
      pure $ User
        { userName      = ucName uc
        , userAccessKey = AccessKey (ucAccessKey uc)
        , userSecretKey = SecretKey (encodeUtf8 (ucSecretKey uc))
        , userPolicies  = policies
        }

-- | Load config from YAML file.
-- Throws an 'IOError' if the file cannot be parsed or if the policy
-- configuration is invalid. The use of 'ioError' / 'userError' makes
-- these failures catchable as 'IOException' rather than crashing via
-- 'Prelude.error'.
loadConfig :: FilePath -> IO ResolvedConfig
loadConfig path = do
  content <- B.readFile path
  case Yaml.decodeEither' content of
    Left err  -> ioError (userError $ "Failed to parse config: " <> Yaml.prettyPrintParseException err)
    Right cfg -> case resolveConfig cfg of
      Left msg -> ioError (userError $ "Configuration error: " <> T.unpack msg)
      Right rc -> pure rc

-- | Default development config.
--
-- WARNING: This configuration is for local development only. It contains
-- hardcoded credentials (access key \"AKID0000000000000000\", secret key
-- \"dev-secret-key-change-me\") that are publicly known. In production,
-- always provide a proper config file via the @--config@ CLI flag and
-- verify that no user uses the default placeholder secret key.
defaultConfig :: ResolvedConfig
defaultConfig = ResolvedConfig
  { rcServer = ServerConfig
    { scHost = "127.0.0.1"
    , scPort = 9443
    , scTlsCert = Nothing
    , scTlsKey = Nothing
    , scDevelopmentMode = True
    }
  , rcStorage = StorageConfig
    { stDataDir = "./data"
    }
  , rcUsers =
    [ User
      { userName = "admin"
      , userAccessKey = AccessKey "AKID0000000000000000"
      , userSecretKey = SecretKey "dev-secret-key-change-me"
      , userPolicies =
        [ Policy Allow [S3AllActions] [ResourceARN "*"]
        ]
      }
    ]
  }
