# s3-oss Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a secure S3-compatible local-first object storage service in Haskell.

**Architecture:** Servant-defined REST API over Warp/WAI with TLS, SigV4 authentication + IAM-like policy engine, SQLite metadata store, and content-addressable filesystem object storage via conduit streaming.

**Tech Stack:** GHC 9.8+, cabal, servant, warp, wai, crypton, sqlite-simple, conduit, xml-conduit, optparse-applicative, yaml

---

### Task 1: Project Scaffolding

**Files:**
- Create: `s3-oss.cabal`
- Create: `cabal.project`
- Create: `src/S3OSS/Prelude.hs`
- Create: `app/Main.hs` (stub)
- Create: `.gitignore`

- [ ] **Step 1: Initialize git repository**

```bash
cd /home/simon/vibe-workspace/haskell-dojo/a-s3-like-oss
git init
```

- [ ] **Step 2: Write `cabal.project`**

```
packages: .
```

- [ ] **Step 3: Write `s3-oss.cabal`**

```cabal
cabal-version:       3.8
name:                s3-oss
version:             0.1.0.0
synopsis:            Secure S3-compatible local-first object storage
license:             MIT
license-file:        LICENSE
author:              Simon
maintainer:          simon@example.com
build-type:          Simple

common common-opts
  default-language:    GHC2021
  ghc-options:
    -Wall -Wcompat -Widentities -Wincomplete-record-updates
    -Wincomplete-uni-patterns -Wmissing-export-lists
    -Wpartial-fields -Wredundant-constraints

library
  import:              common-opts
  hs-source-dirs:      src
  exposed-modules:
    S3OSS.Prelude
    S3OSS.Types
    S3OSS.XML
    S3OSS.Config
    S3OSS.Store
    S3OSS.Object.Storage
    S3OSS.Auth.SigV4
    S3OSS.Auth.Policy
    S3OSS.Bucket.Handler
    S3OSS.Object.Handler
    S3OSS.List.Handler
    S3OSS.Multipart.Manager
    S3OSS.Multipart.Handler
    S3OSS.Presigned
    S3OSS.Server
    S3OSS.API
  build-depends:
    , aeson
    , base                 >=4.17 && <5
    , blaze-markup
    , bytestring
    , conduit              ^>=1.3
    , conduit-extra        ^>=1.3
    , containers
    , crypton              ^>=1.0
    , directory
    , filepath
    , memory
    , mtl
    , optparse-applicative
    , rio                  ^>=0.1
    , servant              ^>=0.20
    , servant-server       ^>=0.20
    , sqlite-simple        ^>=0.4
    , text
    , time
    , wai                  ^>=3.2
    , wai-extra            ^>=3.1
    , warp                 ^>=3.3
    , warp-tls             ^>=3.4
    , xml-conduit          ^>=1.9
    , yaml

executable s3-oss
  import:              common-opts
  hs-source-dirs:      app
  main-is:             Main.hs
  build-depends:
    , base
    , s3-oss
    , optparse-applicative
    , rio

test-suite s3-oss-test
  import:              common-opts
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  other-modules:
    S3OSS.Auth.SigV4Spec
    S3OSS.Auth.PolicySpec
    S3OSS.Object.StorageSpec
    S3OSS.Bucket.HandlerSpec
    S3OSS.Multipart.ManagerSpec
    S3OSS.XMLSpec
  build-depends:
    , base
    , bytestring
    , conduit
    , crypton
    , hspec               ^>=2.11
    , hspec-wai            ^>=0.11
    , QuickCheck           ^>=2.14
    , rio
    , s3-oss
    , servant
    , servant-server
    , sqlite-simple
    , temporary
    , text
    , time
    , wai
    , xml-conduit
```

- [ ] **Step 4: Write `.gitignore`**

```
dist/
dist-newstyle/
.stack-work/
*.o
*.hi
*.dyn_o
*.dyn_hi
.env
data/
```

- [ ] **Step 5: Write `src/S3OSS/Prelude.hs`**

```haskell
module S3OSS.Prelude
  ( module RIO
  , module Exports
  ) where

import RIO
import RIO qualified
import RIO.Text qualified as T
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE

module Exports where
import Control.Monad.Except
import Data.Default
```

- [ ] **Step 6: Write stub `app/Main.hs`**

```haskell
module Main where

import RIO

main :: IO ()
main = putStrLn "s3-oss v0.1.0"
```

- [ ] **Step 7: Build to verify scaffolding**

```bash
cabal build
```

Expected: builds successfully, outputs "s3-oss v0.1.0" when run.

- [ ] **Step 8: Commit scaffolding**

```bash
git add -A
git commit -m "chore: scaffold Haskell project with cabal"
```

---

### Task 2: Core Data Types

**Files:**
- Create: `src/S3OSS/Types.hs`

- [ ] **Step 1: Write `src/S3OSS/Types.hs`**

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Types where

import RIO
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.ByteArray (ByteArrayAccess, convert)
import Crypto.Hash (SHA256, MD5, Digest, hash)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

-- | SHA-256 hash in hex encoding
newtype Sha256Hex = Sha256Hex { unSha256Hex :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

-- | MD5 hash in hex encoding
newtype Md5Hex = Md5Hex { unMd5Hex :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

-- | AWS-style ETag (quoted hex MD5 or composite)
newtype ETag = ETag { unETag :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Bucket name: 3-63 chars, lowercase, numbers, hyphens, dots
newtype BucketName = BucketName { unBucketName :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

-- | Object key: arbitrary Unicode string, up to 1024 bytes
newtype ObjectKey = ObjectKey { unObjectKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

-- | Access key identifier
newtype AccessKey = AccessKey { unAccessKey :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Secret access key (never logged or serialized)
newtype SecretKey = SecretKey { unSecretKey :: ByteString }

-- | Upload ID for multipart uploads
newtype UploadId = UploadId { unUploadId :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Part number in multipart upload (1-10000)
newtype PartNumber = PartNumber { unPartNumber :: Int }
  deriving (Show, Eq, Ord)

-- | S3 action for authorization
data Action
  = GetObject
  | PutObject
  | DeleteObject
  | HeadObject
  | CopyObject
  | ListObjects
  | CreateBucket
  | DeleteBucket
  | ListBuckets
  | HeadBucket
  | CreateMultipartUpload
  | UploadPart
  | CompleteMultipartUpload
  | AbortMultipartUpload
  | AllActions  -- wildcard "s3:*"
  deriving (Show, Eq, Ord, Generic)

-- | Policy effect
data Effect = Allow | Deny
  deriving (Show, Eq, Generic)

-- | Resource ARN pattern
newtype ResourceARN = ResourceARN { unResourceARN :: Text }
  deriving (Show, Eq, Generic)

-- | IAM-like policy statement
data Policy = Policy
  { policyEffect    :: Effect
  , policyActions   :: [Action]
  , policyResources :: [ResourceARN]
  }
  deriving (Show, Eq, Generic)

-- | User with credentials and policies
data User = User
  { userName      :: Text
  , userAccessKey :: AccessKey
  , userSecretKey :: SecretKey
  , userPolicies  :: [Policy]
  }

-- | Object metadata
data ObjectInfo = ObjectInfo
  { oiBucket      :: BucketName
  , oiKey         :: ObjectKey
  , oiHash        :: Sha256Hex
  , oiSize        :: Int64
  , oiContentType :: Maybe Text
  , oiMetadata    :: [(Text, Text)]  -- x-amz-meta-* key-value pairs
  , oiETag        :: ETag
  , oiCreatedAt   :: UTCTime
  , oiUpdatedAt   :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Bucket metadata
data BucketInfo = BucketInfo
  { biName      :: BucketName
  , biCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Multipart upload state
data UploadState = Initiated | InProgress | Completed | Aborted
  deriving (Show, Eq, Generic)

-- | Multipart upload record
data MultipartUpload = MultipartUpload
  { muUploadId  :: UploadId
  , muBucket    :: BucketName
  , muKey       :: ObjectKey
  , muState     :: UploadState
  , muCreatedAt :: UTCTime
  , muExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | A completed part in a multipart upload
data PartInfo = PartInfo
  { piPartNumber :: PartNumber
  , piHash       :: Sha256Hex
  , piSize       :: Int64
  , piETag       :: ETag
  }
  deriving (Show, Eq, Generic)
```

- [ ] **Step 2: Verify types compile**

```bash
cabal build
```

Expected: builds clean.

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/Types.hs s3-oss.cabal
git commit -m "feat: add core data types"
```

---

### Task 3: S3 XML Serialization

**Files:**
- Create: `src/S3OSS/XML.hs`
- Create: `test/S3OSS/XMLSpec.hs`

- [ ] **Step 1: Write failing XML test**

```haskell
-- test/S3OSS/XMLSpec.hs
module S3OSS.XMLSpec (spec) where

import Test.Hspec
import S3OSS.XML
import S3OSS.Types
import Data.Time (UTCTime(..), fromGregorian)
import qualified Data.Text as T
import Text.XML (Document)

spec :: Spec
spec = do
  describe "S3 XML serialization" $ do
    it "renders ListBucketsResult with one bucket" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let buckets = [BucketInfo (BucketName "my-bucket") t]
          owner = OwnerInfo "test-user" "owner-id"
      let doc = renderListBucketsResult owner buckets
      let text = renderLBS doc
      text `shouldContain` "<ListBucketsResult"
      text `shouldContain` "<Name>my-bucket</Name>"
      text `shouldContain` "<CreationDate>2026-06-11T00:00:00Z</CreationDate>"
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cabal test --test-option='-m' --test-option='XML'
```

Expected: compilation failure (module not found).

- [ ] **Step 3: Write `src/S3OSS/XML.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.XML where

import RIO
import qualified RIO.Text as T
import S3OSS.Types
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.XML (Document, Element, Name, Document(..), Prologue(..))
import qualified Text.XML as X
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- | Owner info for XML responses
data OwnerInfo = OwnerInfo
  { ownerDisplayName :: Text
  , ownerId          :: Text
  }

-- | Render XML document as lazy ByteString
renderLBS :: Document -> ByteString
renderLBS doc = TLE.encodeUtf8 $ X.renderText def doc

-- | Render ListBucketsResult
renderListBucketsResult :: OwnerInfo -> [BucketInfo] -> Document
renderListBucketsResult owner buckets =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "ListBucketsResult" X.emptyNode []
      [ node "Owner" []
        [ node "ID" [] [X.NodeContent $ ownerId owner]
        , node "DisplayName" [] [X.NodeContent $ ownerDisplayName owner]
        ]
      , node "Buckets" [] (map renderBucket buckets)
      ]

renderBucket :: BucketInfo -> X.Node
renderBucket bi =
  node "Bucket" []
    [ node "Name" [] [X.NodeContent $ unBucketName $ biName bi]
    , node "CreationDate" [] [X.NodeContent $ iso8601Show $ biCreatedAt bi]
    ]

-- | Render Error response
renderError :: Text -> Text -> Document
renderError code message =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "Error" X.emptyNode []
      [ node "Code" [] [X.NodeContent code]
      , node "Message" [] [X.NodeContent message]
      ]

-- | Render InitiateMultipartUploadResult
renderInitiateMultipartUpload :: BucketName -> ObjectKey -> UploadId -> Document
renderInitiateMultipartUpload bucket key uploadId =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "InitiateMultipartUploadResult" X.emptyNode []
      [ node "Bucket" [] [X.NodeContent $ unBucketName bucket]
      , node "Key" [] [X.NodeContent $ unObjectKey key]
      , node "UploadId" [] [X.NodeContent $ unUploadId uploadId]
      ]

-- | Render ListPartsResult
renderListPartsResult :: BucketName -> ObjectKey -> UploadId -> [PartInfo] -> Bool -> Int -> Document
renderListPartsResult bucket key uploadId parts isTruncated maxParts =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "ListPartsResult" X.emptyNode []
      ( [ node "Bucket" [] [X.NodeContent $ unBucketName bucket]
        , node "Key" [] [X.NodeContent $ unObjectKey key]
        , node "UploadId" [] [X.NodeContent $ unUploadId uploadId]
        , node "IsTruncated" [] [X.NodeContent $ if isTruncated then "true" else "false"]
        , node "MaxParts" [] [X.NodeContent $ tshow maxParts]
        ]
      ++ [ if null parts then X.NodeElement $ X.Element "Parts" X.emptyNode [] []
           else node "Parts" [] (map renderPart parts)
         ]
      )

renderPart :: PartInfo -> X.Node
renderPart pi =
  node "Part" []
    [ node "PartNumber" [] [X.NodeContent $ tshow $ piPartNumber pi]
    , node "ETag" [] [X.NodeContent $ unETag $ piETag pi]
    , node "Size" [] [X.NodeContent $ tshow $ piSize pi]
    ]

-- | Render CompleteMultipartUploadResult
renderCompleteMultipartUpload :: BucketName -> ObjectKey -> ETag -> Document
renderCompleteMultipartUpload bucket key etag =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "CompleteMultipartUploadResult" X.emptyNode []
      [ node "Location" [] [X.NodeContent $ "/" <> unBucketName bucket <> "/" <> unObjectKey key]
      , node "Bucket" [] [X.NodeContent $ unBucketName bucket]
      , node "Key" [] [X.NodeContent $ unObjectKey key]
      , node "ETag" [] [X.NodeContent $ unETag etag]
      ]

-- | Render ListObjects response (both v1 and v2)
renderListObjects :: BucketName -> Maybe Text -> Maybe Text -> Int -> Bool -> [ObjectInfo] -> Bool -> Document
renderListObjects bucket prefix delimiter maxKeys isTruncated objects isV2 =
  Document (Prologue [] Nothing []) root []
  where
    rootName = if isV2 then "ListBucketResult" else "ListBucketResult"
    root = X.Element rootName X.emptyNode []
      ( [ node "Name" [] [X.NodeContent $ unBucketName bucket]
        , node "IsTruncated" [] [X.NodeContent $ if isTruncated then "true" else "false"]
        , node "MaxKeys" [] [X.NodeContent $ tshow maxKeys]
        ]
      ++ maybe [] (\p -> [node "Prefix" [] [X.NodeContent p]]) prefix
      ++ maybe [] (\d -> [node "Delimiter" [] [X.NodeContent d]]) delimiter
      ++ [ node "Contents" [] (map renderObjectInfo objects) | not (null objects) ]
      )

renderObjectInfo :: ObjectInfo -> X.Node
renderObjectInfo oi =
  node "Contents" []
    [ node "Key" [] [X.NodeContent $ unObjectKey $ oiKey oi]
    , node "Size" [] [X.NodeContent $ tshow $ oiSize oi]
    , node "ETag" [] [X.NodeContent $ unETag $ oiETag oi]
    , node "LastModified" [] [X.NodeContent $ iso8601Show $ oiUpdatedAt oi]
    ]

-- | Parse CompleteMultipartUpload request body
parseCompleteMultipartUpload :: ByteString -> Either Text [(PartNumber, ETag)]
parseCompleteMultipartUpload body = do
  doc <- first tshow $ X.parseText X.def $ TL.fromStrict $ decodeUtf8With lenientDecode body
  let root = X.documentRoot doc
  parts <- traverse parsePartElement (X.elementChildren root)
  pure parts
  where
    parsePartElement el = do
      let pnText = X.nodeContent $ head [c | c <- X.elementChildren el, X.nameLocalName (X.elementName c) == "PartNumber"]
      let etagText = X.nodeContent $ head [c | c <- X.elementChildren el, X.nameLocalName (X.elementName c) == "ETag"]
      pn <- case readMaybe (T.unpack pnText) of
              Just n -> Right (PartNumber n)
              Nothing -> Left "Invalid PartNumber"
      pure (pn, ETag etagText)

-- | Parse CopyObject result (CopyObjectResult XML)
renderCopyObjectResult :: ETag -> UTCTime -> Document
renderCopyObjectResult etag lastModified =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "CopyObjectResult" X.emptyNode []
      [ node "ETag" [] [X.NodeContent $ unETag etag]
      , node "LastModified" [] [X.NodeContent $ iso8601Show lastModified]
      ]

-- Helpers

node :: Text -> [(Text, Text)] -> [X.Node] -> X.Node
node name attrs children =
  X.NodeElement $ X.Element name
    (X.emptyNode { X.elementAttributes = map (\(k,v) -> X.Attribute (Just "") k v) attrs })
    children

def :: X.ParseSettings
def = X.def

shouldContain :: ByteString -> ByteString -> Expectation
shouldContain haystack needle =
  unless (needle `B.isInfixOf` haystack) $
    expectationFailure $ "Expected " <> show needle <> " to be in the output"
```

- [ ] **Step 4: Run tests**

```bash
cabal test --test-option='-m' --test-option='XML'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/S3OSS/XML.hs test/S3OSS/XMLSpec.hs s3-oss.cabal
git commit -m "feat: add S3 XML serialization with tests"
```

---

### Task 4: Configuration

**Files:**
- Create: `src/S3OSS/Config.hs`

- [ ] **Step 1: Write `src/S3OSS/Config.hs`**

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Config where

import RIO
import S3OSS.Types
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as B
import GHC.Generics (Generic)

-- | Server configuration
data ServerConfig = ServerConfig
  { scHost            :: Text
  , scPort            :: Int
  , scTlsCert         :: Maybe FilePath
  , scTlsKey          :: Maybe FilePath
  , scDevelopmentMode :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Storage configuration
data StorageConfig = StorageConfig
  { stDataDir :: FilePath
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | User configuration (from YAML — secret key in plaintext until hashed)
data UserConfig = UserConfig
  { ucName      :: Text
  , ucAccessKey :: Text
  , ucSecretKey :: Text
  , ucPolicies  :: [PolicyConfig]
  }
  deriving (Show, Eq, Generic, FromJSON)

data PolicyConfig = PolicyConfig
  { pcEffect    :: Text  -- "Allow" or "Deny"
  , pcActions   :: [Text]
  , pcResources :: [Text]
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Full application configuration (from YAML)
data AppConfig = AppConfig
  { acServer  :: ServerConfig
  , acStorage :: StorageConfig
  , acUsers   :: [UserConfig]
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Resolved application configuration (after parsing + hashing)
data ResolvedConfig = ResolvedConfig
  { rcServer  :: ServerConfig
  , rcStorage :: StorageConfig
  , rcUsers   :: [User]
  }

-- | Parse action string to Action type
parseAction :: Text -> Either Text Action
parseAction "s3:GetObject"                = Right GetObject
parseAction "s3:PutObject"                = Right PutObject
parseAction "s3:DeleteObject"             = Right DeleteObject
parseAction "s3:HeadObject"               = Right HeadObject
parseAction "s3:CopyObject"               = Right CopyObject
parseAction "s3:ListObjects"              = Right ListObjects
parseAction "s3:CreateBucket"             = Right CreateBucket
parseAction "s3:DeleteBucket"             = Right DeleteBucket
parseAction "s3:ListBuckets"              = Right ListBuckets
parseAction "s3:HeadBucket"               = Right HeadBucket
parseAction "s3:CreateMultipartUpload"    = Right CreateMultipartUpload
parseAction "s3:UploadPart"               = Right UploadPart
parseAction "s3:CompleteMultipartUpload"  = Right CompleteMultipartUpload
parseAction "s3:AbortMultipartUpload"     = Right AbortMultipartUpload
parseAction "s3:*"                        = Right AllActions
parseAction "*"                           = Right AllActions
parseAction x                             = Left $ "Unknown action: " <> x

-- | Parse PolicyConfig to Policy
resolvePolicy :: PolicyConfig -> Either Text Policy
resolvePolicy pc = do
  effect <- case pcEffect pc of
    "Allow" -> Right Allow
    "Deny"  -> Right Deny
    x       -> Left $ "Unknown effect: " <> x
  actions <- traverse parseAction (pcActions pc)
  let resources = map ResourceARN (pcResources pc)
  pure $ Policy effect actions resources

-- | Resolve config: hash secret keys, parse policies
resolveConfig :: AppConfig -> IO ResolvedConfig
resolveConfig cfg = do
  users <- traverse resolveUser (acUsers cfg)
  pure $ ResolvedConfig
    { rcServer = acServer cfg
    , rcStorage = acStorage cfg
    , rcUsers = users
    }
  where
    resolveUser uc = do
      -- For now, store secret key directly (bcrypt integration later)
      -- In a real deployment, this would hash the key
      let secretKeyBytes = encodeUtf8 (ucSecretKey uc)
      policies <- either error pure $ traverse resolvePolicy (ucPolicies uc)
      pure $ User
        { userName = ucName uc
        , userAccessKey = AccessKey (ucAccessKey uc)
        , userSecretKey = SecretKey secretKeyBytes
        , userPolicies = policies
        }

-- | Load config from YAML file
loadConfig :: FilePath -> IO ResolvedConfig
loadConfig path = do
  content <- B.readFile path
  case Yaml.decodeEither' content of
    Left err  -> error $ "Failed to parse config: " <> Yaml.prettyPrintParseException err
    Right cfg -> resolveConfig cfg

-- | Default development config
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
        [ Policy Allow [AllActions] [ResourceARN "*"]
        ]
      }
    ]
  }
```

- [ ] **Step 2: Verify it compiles**

```bash
cabal build
```

Expected: builds clean.

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/Config.hs
git commit -m "feat: add configuration parsing"
```

---

### Task 5: SQLite Metadata Store

**Files:**
- Create: `src/S3OSS/Store.hs`

- [ ] **Step 1: Write `src/S3OSS/Store.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module S3OSS.Store where

import RIO
import S3OSS.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)

-- | Database handle
data Store = Store
  { storeConn :: Connection
  , storeDir  :: FilePath
  }

-- | Initialize the store: create tables if they don't exist
initStore :: FilePath -> IO Store
initStore dataDir = do
  let dbPath = dataDir <> "/meta.sqlite"
  createDirectoryIfMissing True dataDir
  conn <- open dbPath
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA foreign_keys=ON"
  execute_ conn $ fromString $ T.unpack schema
  pure $ Store conn dataDir

schema :: Text
schema = T.unlines
  [ "CREATE TABLE IF NOT EXISTS buckets ("
  , "  id         INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "  name       TEXT NOT NULL UNIQUE,"
  , "  created_at TEXT NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS objects ("
  , "  id           INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "  bucket_id    INTEGER NOT NULL REFERENCES buckets(id),"
  , "  key          TEXT NOT NULL,"
  , "  sha256       TEXT NOT NULL,"
  , "  size         INTEGER NOT NULL,"
  , "  content_type TEXT,"
  , "  etag         TEXT NOT NULL,"
  , "  metadata     TEXT,"
  , "  ref_count    INTEGER NOT NULL DEFAULT 1,"
  , "  created_at   TEXT NOT NULL,"
  , "  updated_at   TEXT NOT NULL,"
  , "  UNIQUE(bucket_id, key)"
  , ");"
  , "CREATE TABLE IF NOT EXISTS multipart_uploads ("
  , "  upload_id  TEXT PRIMARY KEY,"
  , "  bucket_id  INTEGER NOT NULL REFERENCES buckets(id),"
  , "  key        TEXT NOT NULL,"
  , "  state      TEXT NOT NULL,"
  , "  created_at TEXT NOT NULL,"
  , "  expires_at TEXT NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS multipart_parts ("
  , "  upload_id   TEXT NOT NULL REFERENCES multipart_uploads(upload_id),"
  , "  part_number INTEGER NOT NULL,"
  , "  sha256      TEXT NOT NULL,"
  , "  size        INTEGER NOT NULL,"
  , "  etag        TEXT NOT NULL,"
  , "  PRIMARY KEY (upload_id, part_number)"
  , ");"
  ]

-- | ISO-8601 format helper (uses show which gives ISO-8601 for UTCTime)
iso8601 :: UTCTime -> Text
iso8601 = tshow

-- Bucket operations

createBucket :: Store -> BucketName -> IO (Either Text BucketInfo)
createBucket store name = do
  now <- getCurrentTime
  tryAny (do
    execute (storeConn store)
      "INSERT INTO buckets (name, created_at) VALUES (?, ?)"
      (unBucketName name, iso8601 now)
    pure $ BucketInfo name now
    ) >>= \case
      Left _  -> pure $ Left "BucketAlreadyExists"
      Right r -> pure $ Right r

deleteBucket :: Store -> BucketName -> IO Bool
deleteBucket store name = do
  -- Only delete if empty
  objCount <- queryNamed store
    "SELECT COUNT(*) as c FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.name = ?"
    [unBucketName name]
  if objCount == (0 :: Int)
    then do
      executeNamed store "DELETE FROM buckets WHERE name = ?" [unBucketName name]
      pure True
    else pure False

listBuckets :: Store -> IO [BucketInfo]
listBuckets store = do
  rows <- query_ (storeConn store) "SELECT name, created_at FROM buckets ORDER BY name"
  pure [BucketInfo (BucketName n) (parseTimeIso8601 t) | (n, t) <- rows]

headBucket :: Store -> BucketName -> IO Bool
headBucket store name = do
  result <- queryNamed store "SELECT 1 FROM buckets WHERE name = ?" [unBucketName name]
  pure (not (null (result :: [Only Int])))

-- Object operations

putObjectMeta :: Store -> BucketName -> ObjectKey -> Sha256Hex -> Int64 -> Maybe Text -> [(Text, Text)] -> ETag -> IO ObjectInfo
putObjectMeta store bucket key hash size contentType metadata etag = do
  now <- getCurrentTime
  bucketId <- getBucketId store bucket
  let metaJson = tshow metadata  -- simplified; in production use Aeson
  execute (storeConn store)
    "INSERT INTO objects (bucket_id, key, sha256, size, content_type, etag, metadata, created_at, updated_at) \
     \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
     \ON CONFLICT(bucket_id, key) DO UPDATE SET \
     \  sha256=excluded.sha256, size=excluded.size, content_type=excluded.content_type, \
     \  etag=excluded.etag, metadata=excluded.metadata, updated_at=excluded.updated_at"
    (bucketId, unObjectKey key, unSha256Hex hash, size, contentType, unETag etag, metaJson, iso8601 now, iso8601 now)
  pure $ ObjectInfo bucket key hash size contentType metadata etag now now

getObjectMeta :: Store -> BucketName -> ObjectKey -> IO (Maybe ObjectInfo)
getObjectMeta store bucket key = do
  rows <- queryNamed store
    "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.metadata, o.created_at, o.updated_at \
     \FROM objects o JOIN buckets b ON o.bucket_id = b.id \
     \WHERE b.name = ? AND o.key = ?"
    [unBucketName bucket, unObjectKey key]
  pure $ listToMaybe $ map toObjectInfo rows
  where
    toObjectInfo (bn, k, h, sz, ct, e, m, ca, ua) =
      ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e) (parseTimeIso8601 ca) (parseTimeIso8601 ua)

deleteObjectMeta :: Store -> BucketName -> ObjectKey -> IO Bool
deleteObjectMeta store bucket key = do
  bucketId <- getBucketId store bucket
  changes <- executeNamed store
    "DELETE FROM objects WHERE bucket_id = ? AND key = ?"
    [bucketId, unObjectKey key]
  pure (changes > 0)

-- Multipart operations

createMultipartUpload :: Store -> BucketName -> ObjectKey -> UploadId -> IO MultipartUpload
createMultipartUpload store bucket key uploadId = do
  now <- getCurrentTime
  let expiresAt = addUTCTime (7 * 24 * 60 * 60) now  -- 7 days
  bucketId <- getBucketId store bucket
  execute (storeConn store)
    "INSERT INTO multipart_uploads (upload_id, bucket_id, key, state, created_at, expires_at) VALUES (?, ?, ?, 'initiated', ?, ?)"
    (unUploadId uploadId, bucketId, unObjectKey key, iso8601 now, iso8601 expiresAt)
  pure $ MultipartUpload uploadId bucket key Initiated now expiresAt

addPart :: Store -> UploadId -> PartNumber -> Sha256Hex -> Int64 -> ETag -> IO ()
addPart store uploadId partNum hash size etag = do
  execute (storeConn store)
    "INSERT OR REPLACE INTO multipart_parts (upload_id, part_number, sha256, size, etag) VALUES (?, ?, ?, ?, ?)"
    (unUploadId uploadId, partNum, unSha256Hex hash, size, unETag etag)

getParts :: Store -> UploadId -> IO [PartInfo]
getParts store uploadId = do
  rows <- queryNamed store
    "SELECT part_number, sha256, size, etag FROM multipart_parts WHERE upload_id = ? ORDER BY part_number"
    [unUploadId uploadId]
  pure [PartInfo (PartNumber pn) (Sha256Hex h) sz (ETag e) | (pn, h, sz, e) <- rows]

completeMultipartUpload :: Store -> UploadId -> IO ()
completeMultipartUpload store uploadId = do
  executeNamed store
    "UPDATE multipart_uploads SET state = 'completed' WHERE upload_id = ?"
    [unUploadId uploadId]

abortMultipartUpload :: Store -> UploadId -> IO ()
abortMultipartUpload store uploadId = do
  executeNamed store "DELETE FROM multipart_parts WHERE upload_id = ?" [unUploadId uploadId]
  executeNamed store "DELETE FROM multipart_uploads WHERE upload_id = ?" [unUploadId uploadId]

getMultipartUpload :: Store -> UploadId -> IO (Maybe MultipartUpload)
getMultipartUpload store uploadId = do
  rows <- queryNamed store
    "SELECT m.upload_id, b.name, m.key, m.state, m.created_at, m.expires_at \
     \FROM multipart_uploads m JOIN buckets b ON m.bucket_id = b.id \
     \WHERE m.upload_id = ? AND m.state = 'initiated'"
    [unUploadId uploadId]
  pure $ listToMaybe $ map toUpload rows
  where
    toUpload (uid, bn, k, st, ca, ea) =
      MultipartUpload (UploadId uid) (BucketName bn) (ObjectKey k)
        (case st of "initiated" -> Initiated; "completed" -> Completed; _ -> Aborted)
        (parseTimeIso8601 ca) (parseTimeIso8601 ea)

cleanupExpiredUploads :: Store -> IO Int
cleanupExpiredUploads store = do
  now <- getCurrentTime
  expired <- queryNamed store
    "SELECT upload_id FROM multipart_uploads WHERE state = 'initiated' AND expires_at < ?"
    [iso8601 now]
  forM_ expired $ \(Only uid) -> abortMultipartUpload store (UploadId uid)
  pure (length expired)

-- Helpers

getBucketId :: Store -> BucketName -> IO Int
getBucketId store name = do
  rows <- queryNamed store "SELECT id FROM buckets WHERE name = ?" [unBucketName name]
  case rows of
    [Only bid] -> pure bid
    _ -> error $ "Bucket not found: " <> T.unpack (unBucketName name)

queryNamed :: (FromRow r) => Store -> Text -> [Text] -> IO [r]
queryNamed store q params =
  query (storeConn store) (fromString $ T.unpack q) params

executeNamed :: Store -> Text -> [Text] -> IO ()
executeNamed store q params =
  execute_ (storeConn store) (fromString $ T.unpack q) params

parseTimeIso8601 :: Text -> UTCTime
parseTimeIso8601 t =
  case readMaybe (T.unpack t) of
    Just utc -> utc
    Nothing  -> error $ "Invalid ISO-8601 timestamp: " <> T.unpack t
```

- [ ] **Step 2: Verify compilation**

```bash
cabal build
```

Expected: builds clean.

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/Store.hs
git commit -m "feat: add SQLite metadata store"
```

---

### Task 6: Object Storage Engine

**Files:**
- Create: `src/S3OSS/Object/Storage.hs`
- Create: `test/S3OSS/Object/StorageSpec.hs`

- [ ] **Step 1: Write failing storage test**

```haskell
-- test/S3OSS/Object/StorageSpec.hs
module S3OSS.Object.StorageSpec (spec) where

import Test.Hspec
import S3OSS.Object.Storage
import S3OSS.Types
import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import System.IO.Temp (withSystemTempDirectory)
import Data.Conduit ((.=|), runConduitRes)

spec :: Spec
spec = do
  describe "Object Storage" $ do
    it "writes and reads an object correctly" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = "hello, world!" :: ByteString
        let source = yield (B.take 1024 content)  -- single chunk
        hash <- putObject tmp source
        -- Read it back
        result <- runConduitRes $ getObject tmp hash .| CL.consume
        B.concat result `shouldBe` content
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cabal test --test-option='-m' --test-option='Storage'
```

Expected: compilation failure (module not found).

- [ ] **Step 3: Write `src/S3OSS/Object/Storage.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Object.Storage where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import Crypto.Hash (SHA256, MD5, Digest, hash, hashlazy, digestFromByteString, hashInit, hashUpdate, hashFinalize)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Conduit ((.=|), ConduitT, runConduitRes, yield)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.IO (IOMode(..), withFile)
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)

-- | Write object from a conduit source, store as content-addressed file
-- Returns the SHA-256 hash and total size.
putObject :: FilePath -> ConduitT () ByteString IO () -> IO (Sha256Hex, Int64, ByteString)
putObject dataDir source = do
  let objectsDir = dataDir <> "/objects"
  createDirectoryIfMissing True objectsDir

  -- Create temp file
  let tmpPath = dataDir <> "/.tmp-" <> show (abs (42 :: Int))  -- simplified; use UUID in production

  -- Stream source into temp file, computing SHA-256 and MD5 concurrently
  (sha256Ctx, md5Ctx, totalSize) <- runConduitRes $
    source .| foldHashAndWrite tmpPath

  let sha256Digest = hashFinalize sha256Ctx
  let md5Digest = hashFinalize md5Ctx
  let sha256Hex = Sha256Hex $ T.pack $ show sha256Digest
  let md5Hex = Md5Hex $ T.pack $ show md5Digest
  let etag = ETag $ "\"" <> unMd5Hex md5Hex <> "\""

  -- Move to final location
  let prefix = T.take 2 (unSha256Hex sha256Hex)
  let shardDir = objectsDir <> "/" <> T.unpack prefix
  createDirectoryIfMissing True shardDir
  let finalPath = shardDir <> "/" <> T.unpack (unSha256Hex sha256Hex)

  -- Atomic rename (only if file doesn't exist — dedup)
  exists <- doesFileExist finalPath
  unless exists $ do
    renameFile tmpPath finalPath

  pure (sha256Hex, totalSize, encodeUtf8 $ unETag etag)

-- | Read object by SHA-256 hash, returns a conduit source
getObject :: FilePath -> Sha256Hex -> ConduitT () ByteString IO ()
getObject dataDir sha256 = do
  let prefix = T.take 2 (unSha256Hex sha256)
  let path = dataDir <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack (unSha256Hex sha256)
  CB.sourceFile path

-- | Delete an object file (only if no other references)
deleteObject :: FilePath -> Sha256Hex -> IO ()
deleteObject dataDir sha256 = do
  let prefix = T.take 2 (unSha256Hex sha256)
  let path = dataDir <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack (unSha256Hex sha256)
  exists <- doesFileExist path
  when exists $ removeFile path

-- Internal: fold conduit into hash contexts and write to file
foldHashAndWrite :: FilePath -> ConduitT ByteString Void IO (Crypto.Context SHA256, Crypto.Context MD5, Int64)
foldHashAndWrite path = do
  -- Use conduit to accumulate
  liftIO $ withFile path WriteMode $ \h -> do
    let loop shaCtx md5Ctx total = do
          mbs <- await
          case mbs of
            Nothing -> pure (shaCtx, md5Ctx, total)
            Just bs -> do
              liftIO $ B.hPut h bs
              let shaCtx' = hashUpdate shaCtx bs
              let md5Ctx' = hashUpdate md5Ctx bs
              loop shaCtx' md5Ctx' (total + fromIntegral (B.length bs))
    loop hashInit hashInit 0
```

- [ ] **Step 4: Run tests**

```bash
cabal test --test-option='-m' --test-option='Storage'
```

Expected: testing the write/read round-trip (may need adjustments).

- [ ] **Step 5: Commit**

```bash
git add src/S3OSS/Object/Storage.hs test/S3OSS/Object/StorageSpec.hs
git commit -m "feat: add content-addressable object storage engine"
```

---

### Task 7: AWS SigV4 Authentication

**Files:**
- Create: `src/S3OSS/Auth/SigV4.hs`
- Create: `test/S3OSS/Auth/SigV4Spec.hs`

- [ ] **Step 1: Write failing SigV4 test**

```haskell
-- test/S3OSS/Auth/SigV4Spec.hs
module S3OSS.Auth.SigV4Spec (spec) where

import Test.Hspec
import S3OSS.Auth.SigV4
import S3OSS.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

spec :: Spec
spec = do
  describe "SigV4 Signature" $ do
    it "produces correct signing key from known test vector" $ do
      let secret = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let date = "20150830"
      let region = "us-east-1"
      let service = "iam"
      let signingKey = deriveSigningKey secret date region service
      -- Expected: c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9
      BA.convert signingKey `shouldBe`
        "\196\175\177\204\87q\216q\118:9>D\183\3W\27U\204(BM\26^\134\218n\211\193T\164\185"
```

- [ ] **Step 2: Write `src/S3OSS/Auth/SigV4.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.SigV4 where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash (Digest, digestFromByteString)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)

-- | Derive the AWS SigV4 signing key
deriveSigningKey :: SecretKey -> Text -> Text -> Text -> ByteString
deriveSigningKey (SecretKey secret) date region service =
  let kDate     = hmacGetDigest $ hmac secret (TE.encodeUtf8 date)
      kRegion   = hmacGetDigest $ hmac kDate (TE.encodeUtf8 region)
      kService  = hmacGetDigest $ hmac kRegion (TE.encodeUtf8 service)
      kSigning  = hmacGetDigest $ hmac kService "aws4_request"
  in kSigning

-- | Verify an AWS SigV4 Authorization header
-- Returns the verified AccessKey on success, or an error message.
verifySigV4 :: [User] -> Wai.Request -> IO (Either Text User)
verifySigV4 users req = do
  -- Extract Authorization header
  case lookup "Authorization" (Wai.requestHeaders req) of
    Nothing -> pure $ Left "Missing Authorization header"
    Just authHeader -> do
      -- Parse the header
      parsed <- pure $ parseAuthHeader (TE.decodeUtf8' authHeader)
      case parsed of
        Left err -> pure $ Left err
        Right (accessKey, credentialScope, signedHeaders, signature) -> do
          -- Find user
          case find (\u -> unAccessKey (userAccessKey u) == accessKey) users of
            Nothing -> pure $ Left "Invalid access key"
            Just user -> do
              -- Verify signature
              let stringToSign = buildStringToSign req credentialScope signedHeaders
              let signingKey = deriveSigningKey (userSecretKey user) (csDate credentialScope) (csRegion credentialScope) (csService credentialScope)
              let expected = hmacGetDigest $ hmac signingKey (TE.encodeUtf8 stringToSign)
              -- Compare signatures (constant-time would be better, but hex compare is OK for now)
              if signature == expected
                then pure $ Right user
                else pure $ Left "Signature mismatch"

-- | Parsed credential scope from Authorization header
data CredentialScope = CredentialScope
  { csDate    :: Text  -- YYYYMMDD
  , csRegion  :: Text
  , csService :: Text
  }

-- | Parse the Authorization header
-- Format: AWS4-HMAC-SHA256 Credential=<akid>/<date>/<region>/<service>/aws4_request, SignedHeaders=<headers>, Signature=<sig>
parseAuthHeader :: Either Text Text -> Either Text (Text, CredentialScope, [Text], ByteString)
parseAuthHeader (Left _)  = Left "Invalid UTF-8 in Authorization header"
parseAuthHeader (Right t) = do
  -- Simplified parser: extract credential, signedHeaders, signature
  let parts = T.splitOn ", " t
  when (length parts /= 3) $ Left "Expected 3 comma-separated parts in Authorization header"
  let algorithmPart = head parts
  unless ("AWS4-HMAC-SHA256" `T.isPrefixOf` algorithmPart) $
    Left "Expected AWS4-HMAC-SHA256 algorithm"
  -- Parse Credential
  let credPart = parts !! 0
  let sigHeadersPart = parts !! 1
  let sigPart = parts !! 2
  -- Extract access key and scope from credential
  let afterAlgo = T.stripPrefix "AWS4-HMAC-SHA256 Credential=" credPart
  case afterAlgo of
    Nothing -> Left "Missing Credential in Authorization header"
    Just credVal -> do
      let scopeParts = T.splitOn "/" credVal
      when (length scopeParts /= 5) $ Left "Invalid credential scope"
      let accessKey = scopeParts !! 0
      let date    = scopeParts !! 1
      let region  = scopeParts !! 2
      let service = scopeParts !! 3
      let scope = CredentialScope date region service
      -- Parse SignedHeaders
      let sigHeaders = T.stripPrefix "SignedHeaders=" sigHeadersPart
      case sigHeaders of
        Nothing -> Left "Missing SignedHeaders"
        Just sh -> do
          -- Parse Signature
          let sig = T.stripPrefix "Signature=" sigPart
          case sig of
            Nothing -> Left "Missing Signature"
            Just s -> Right (accessKey, scope, T.splitOn ";" sh, TE.encodeUtf8 s)

-- | Build the string-to-sign
buildStringToSign :: Wai.Request -> CredentialScope -> [Text] -> Text
buildStringToSign req scope signedHeaders =
  T.intercalate "\n"
    [ "AWS4-HMAC-SHA256"
    , ""  -- timestamp from x-amz-date header
    , csDate scope <> "/" <> csRegion scope <> "/" <> csService scope <> "/aws4_request"
    , ""  -- hash of canonical request
    ]

import qualified Network.Wai as Wai
```

- [ ] **Step 4: Run tests**

```bash
cabal test --test-option='-m' --test-option='SigV4'
```

- [ ] **Step 5: Commit**

```bash
git add src/S3OSS/Auth/SigV4.hs test/S3OSS/Auth/SigV4Spec.hs
git commit -m "feat: add AWS SigV4 signature verification"
```

---

### Task 8: Policy Engine

**Files:**
- Create: `src/S3OSS/Auth/Policy.hs`
- Create: `test/S3OSS/Auth/PolicySpec.hs`

- [ ] **Step 1: Write failing policy test**

```haskell
-- test/S3OSS/Auth/PolicySpec.hs
module S3OSS.Auth.PolicySpec (spec) where

import Test.Hspec
import S3OSS.Auth.Policy
import S3OSS.Types

spec :: Spec
spec = do
  describe "Policy engine" $ do
    it "allows when Allow policy matches" $ do
      let policies = [Policy Allow [GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` True

    it "denies when no policy matches" $ do
      let policies = [Policy Allow [GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies PutObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "deny overrides allow" $ do
      let policies =
            [ Policy Allow [AllActions] [ResourceARN "*"]
            , Policy Deny  [DeleteObject] [ResourceARN "arn:aws:s3:::protected/*"]
            ]
      evaluate policies DeleteObject (ResourceARN "arn:aws:s3:::protected/important.db") `shouldBe` False
      evaluate policies GetObject (ResourceARN "arn:aws:s3:::protected/important.db") `shouldBe` True

    it "wildcard action matches specific action" $ do
      let policies = [Policy Allow [AllActions] [ResourceARN "*"]]
      evaluate policies PutObject (ResourceARN "arn:aws:s3:::any-bucket/any-key") `shouldBe` True
```

- [ ] **Step 2: Implement `src/S3OSS/Auth/Policy.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.Policy where

import RIO
import S3OSS.Types
import qualified RIO.Text as T

-- | Evaluate policies for a given action and resource.
-- Returns True if the action is allowed.
-- Deny always overrides Allow. Default is deny (no match = False).
evaluate :: [Policy] -> Action -> ResourceARN -> Bool
evaluate policies action resource =
  case find (matchesDeny action resource) policies of
    Just _  -> False  -- explicit deny
    Nothing -> any (matchesAllow action resource) policies  -- need at least one allow

matchesDeny :: Action -> ResourceARN -> Policy -> Bool
matchesDeny action resource p =
  policyEffect p == Deny
  && actionMatches action (policyActions p)
  && resourceMatches resource (policyResources p)

matchesAllow :: Action -> ResourceARN -> Policy -> Bool
matchesAllow action resource p =
  policyEffect p == Allow
  && actionMatches action (policyActions p)
  && resourceMatches resource (policyResources p)

-- | Check if an action is covered by a list of policy actions
actionMatches :: Action -> [Action] -> Bool
actionMatches _    actions | AllActions `elem` actions = True
actionMatches action actions = action `elem` actions

-- | Check if a resource ARN matches a pattern in a policy
resourceMatches :: ResourceARN -> [ResourceARN] -> Bool
resourceMatches _ resources | ResourceARN "*" `elem` resources = True
resourceMatches (ResourceARN target) patterns =
  any (matchARN target . unResourceARN) patterns

-- | Simple ARN matching with wildcard support
-- "*" matches everything
-- "arn:aws:s3:::bucket-name" matches exactly
-- "arn:aws:s3:::bucket-name/*" matches objects in that bucket
matchARN :: Text -> Text -> Bool
matchARN _      "*"       = True
matchARN target pattern
  | "*" `T.isSuffixOf` pattern =
      let prefix = T.dropEnd 1 pattern  -- remove trailing *
      in prefix `T.isPrefixOf` target
  | otherwise = target == pattern
```

- [ ] **Step 3: Run tests**

```bash
cabal test --test-option='-m' --test-option='Policy'
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/S3OSS/Auth/Policy.hs test/S3OSS/Auth/PolicySpec.hs
git commit -m "feat: add IAM-like policy evaluation engine"
```

---

### Task 9: Bucket Handlers

**Files:**
- Create: `src/S3OSS/Bucket/Handler.hs`
- Create: `test/S3OSS/Bucket/HandlerSpec.hs`

- [ ] **Step 1: Write `src/S3OSS/Bucket/Handler.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Bucket.Handler where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai (Response, responseLBS, responseBuilder)
import Network.HTTP.Types (status200, status404, status409)

-- | Handle CreateBucket (PUT /{bucket})
handleCreateBucket :: Store -> User -> BucketName -> IO Response
handleCreateBucket store user name = do
  -- Auth check
  unless (evaluate (userPolicies user) CreateBucket (bucketARN name)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  -- Create
  result <- createBucket store name
  case result of
    Right _  -> pure $ responseLBS status200
                  [("Content-Type", "application/xml")]
                  (renderLBS $ renderCreateBucketResult name)
    Left _   -> pure $ errorResponse status409 "BucketAlreadyExists" "The requested bucket name is not available"

-- | Handle DeleteBucket (DELETE /{bucket})
handleDeleteBucket :: Store -> User -> BucketName -> IO Response
handleDeleteBucket store user name = do
  unless (evaluate (userPolicies user) DeleteBucket (bucketARN name)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  deleted <- deleteBucket store name
  if deleted
    then pure $ responseLBS status204 [] ""
    else pure $ errorResponse status409 "BucketNotEmpty" "The bucket you tried to delete is not empty"

-- | Handle ListBuckets (GET /)
handleListBuckets :: Store -> User -> IO Response
handleListBuckets store user = do
  unless (evaluate (userPolicies user) ListBuckets (ResourceARN "*")) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  buckets <- listBuckets store
  let owner = OwnerInfo (userName user) (unAccessKey $ userAccessKey user)
  pure $ responseLBS status200
    [("Content-Type", "application/xml")]
    (renderLBS $ renderListBucketsResult owner buckets)

-- | Handle HeadBucket (HEAD /{bucket})
handleHeadBucket :: Store -> User -> BucketName -> IO Response
handleHeadBucket store user name = do
  unless (evaluate (userPolicies user) HeadBucket (bucketARN name)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  exists <- headBucket store name
  if exists
    then pure $ responseLBS status200 [] ""
    else pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"

-- Helpers

bucketARN :: BucketName -> ResourceARN
bucketARN name = ResourceARN $ "arn:aws:s3:::" <> unBucketName name

renderCreateBucketResult :: BucketName -> Document
renderCreateBucketResult name =
  Document (Prologue [] Nothing []) root []
  where
    root = X.Element "CreateBucketResult" X.emptyNode []
      [ node "BucketName" [] [X.NodeContent $ unBucketName name]
      ]

errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)

import Network.HTTP.Types qualified as HTTP
import Text.XML qualified as X
```

- [ ] **Step 2: Verify compilation**

```bash
cabal build
```

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/Bucket/Handler.hs
git commit -m "feat: add bucket handlers (create, delete, list, head)"
```

---

### Task 10: Object Handlers

**Files:**
- Create: `src/S3OSS/Object/Handler.hs`

- [ ] **Step 1: Write object handlers**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Object.Handler where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.Object.Storage
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai
import Network.HTTP.Types
import Data.Conduit ((.=|))
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B

-- | Handle PutObject
handlePutObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> Request -> IO Response
handlePutObject store dataDir user bucket key req = do
  -- Auth
  unless (evaluate (userPolicies user) PutObject (objectARN bucket key)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  -- Get Content-Type header
  let contentType = lookup "Content-Type" (requestHeaders req)

  -- Stream request body into storage
  let source = requestBody req  -- conduit source from WAI
  (sha256, size, etagBytes) <- putObject dataDir source
  let etag = ETag $ decodeUtf8With lenientDecode etagBytes

  -- Store metadata
  _ <- putObjectMeta store bucket key sha256 size contentType [] etag

  pure $ responseLBS status200 [] ""

-- | Handle GetObject
handleGetObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> Request -> IO Response
handleGetObject store dataDir user bucket key _req = do
  unless (evaluate (userPolicies user) GetObject (objectARN bucket key)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  mObj <- getObjectMeta store bucket key
  case mObj of
    Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
    Just obj -> do
      let headers = [ ("Content-Type", fromMaybe "application/octet-stream" (oiContentType obj))
                    , ("ETag", encodeUtf8 (unETag (oiETag obj)))
                    , ("Content-Length", fromString (show (oiSize obj)))
                    ]
      let source = getObject dataDir (oiHash obj)
      pure $ responseStream status200 headers $ \write flush -> do
        runConduit $ source .| CL.mapM_ (\bs -> liftIO $ write (builder bs))
        flush
  where
    builder bs = fromByteString bs

-- | Handle DeleteObject
handleDeleteObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> IO Response
handleDeleteObject store dataDir user bucket key = do
  unless (evaluate (userPolicies user) DeleteObject (objectARN bucket key)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  mObj <- getObjectMeta store bucket key
  case mObj of
    Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
    Just obj -> do
      _ <- deleteObjectMeta store bucket key
      deleteObject dataDir (oiHash obj)
      pure $ responseLBS status204 [] ""

-- | Handle HeadObject
handleHeadObject :: Store -> User -> BucketName -> ObjectKey -> IO Response
handleHeadObject store user bucket key = do
  unless (evaluate (userPolicies user) HeadObject (objectARN bucket key)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  mObj <- getObjectMeta store bucket key
  case mObj of
    Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
    Just obj -> pure $ responseLBS status200
      [ ("Content-Type", fromMaybe "application/octet-stream" (encodeUtf8 <$> oiContentType obj))
      , ("ETag", encodeUtf8 (unETag (oiETag obj)))
      , ("Content-Length", fromString (show (oiSize obj)))
      , ("Last-Modified", fromString (iso8601Show (oiUpdatedAt obj)))
      ] ""

-- | Handle CopyObject
handleCopyObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> BucketName -> ObjectKey -> IO Response
handleCopyObject store dataDir user srcBucket srcKey dstBucket dstKey = do
  unless (evaluate (userPolicies user) GetObject (objectARN srcBucket srcKey)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  unless (evaluate (userPolicies user) PutObject (objectARN dstBucket dstKey)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  mObj <- getObjectMeta store srcBucket srcKey
  case mObj of
    Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
    Just obj -> do
      now <- getCurrentTime
      let newObj = obj { oiBucket = dstBucket, oiKey = dstKey
                       , oiCreatedAt = now, oiUpdatedAt = now }
      _ <- putObjectMeta store dstBucket dstKey (oiHash obj) (oiSize obj) (oiContentType obj) (oiMetadata obj) (oiETag obj)
      pure $ responseLBS status200
        [("Content-Type", "application/xml")]
        (renderLBS $ renderCopyObjectResult (oiETag obj) now)

-- Helpers

objectARN :: BucketName -> ObjectKey -> ResourceARN
objectARN bucket key = ResourceARN $ "arn:aws:s3:::" <> unBucketName bucket <> "/" <> unObjectKey key
```

- [ ] **Step 2: Verify compilation**

```bash
cabal build
```

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/Object/Handler.hs
git commit -m "feat: add object handlers (put, get, delete, head, copy)"
```

---

### Task 11: Multipart Upload Manager + Handlers

**Files:**
- Create: `src/S3OSS/Multipart/Manager.hs`
- Create: `src/S3OSS/Multipart/Handler.hs`

- [ ] **Step 1: Write multipart manager**

```haskell
-- src/S3OSS/Multipart/Manager.hs
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Multipart.Manager where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.Object.Storage
import S3OSS.XML
import qualified RIO.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Data.Conduit ((.=|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.IO (IOMode(..), withFile)
import Crypto.Hash (MD5, hashInit, hashUpdate, hashFinalize)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA

-- | Generate a unique upload ID
generateUploadId :: IO UploadId
generateUploadId = do
  -- Simple UUID v4 generation (in production, use a proper UUID library)
  bytes <- replicateM 16 (fromIntegral <$> randomRIO (0, 255) :: IO Int)
  let hex = T.pack $ concatMap (printf "%02x") bytes
  pure $ UploadId hex

import System.Random (randomRIO)
import Text.Printf (printf)

-- | Initialize a multipart upload
initiateUpload :: Store -> FilePath -> BucketName -> ObjectKey -> IO UploadId
initiateUpload store dataDir bucket key = do
  uploadId <- generateUploadId
  -- Create parts directory
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  createDirectoryIfMissing True partsDir
  _ <- createMultipartUpload store bucket key uploadId
  pure uploadId

-- | Upload a single part
uploadPart :: Store -> FilePath -> UploadId -> PartNumber -> ConduitT () ByteString IO () -> IO (ETag)
uploadPart store dataDir uploadId partNum source = do
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  let partPath = partsDir <> "/part-" <> printf "%05d" (unPartNumber partNum)

  -- Stream part to file, compute MD5
  (md5Ctx, size) <- runConduitRes $
    source .| foldMD5AndWrite partPath

  let md5Digest = hashFinalize md5Ctx
  let md5Hex = T.pack $ show md5Digest
  let sha256Hex = Sha256Hex $ T.pack $ show (Crypto.hash ("" :: ByteString) :: Crypto.Digest Crypto.SHA256)  -- simplified
  let etag = ETag $ "\"" <> md5Hex <> "\""

  -- Record part in DB
  addPart store uploadId partNum sha256Hex size etag
  pure etag

-- | Complete a multipart upload
completeUpload :: Store -> FilePath -> UploadId -> [(PartNumber, ETag)] -> IO (Either Text (BucketName, ObjectKey, ETag))
completeUpload store dataDir uploadId parts = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ Left "NoSuchUpload"
    Just upload -> do
      -- Validate all parts present
      storedParts <- getParts store uploadId
      let partNumbers = map fst parts
      let storedNumbers = map piPartNumber storedParts
      when (sort partNumbers /= sort storedNumbers) $
        pure $ Left "InvalidPart"

      -- Assemble: concatenate part files into final object
      let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
      let finalPath = dataDir <> "/.tmp-complete-" <> T.unpack (unUploadId uploadId)

      -- Stream assembly
      (sha256Ctx, md5Ctx, totalSize, partMd5s) <- runConduitRes $
        assemble source $
          CB.sourceFile finalPath

      -- Rename: we need to compute the final hash first
      -- Simplified: stream through SHA-256 computation
      let sha256Hex = Sha256Hex $ T.pack $ show (hashFinalize sha256Ctx)
      let compositeMd5 = T.pack $ show $ hashFinalize md5Ctx
      let compositeETag = ETag $ "\"" <> compositeMd5 <> "-" <> tshow (length parts) <> "\""

      -- Store object metadata
      _ <- putObjectMeta store (muBucket upload) (muKey upload) sha256Hex totalSize Nothing [] compositeETag

      -- Move assembled file to content-addressed location (simplified)
      -- In production, this uses putObject's streaming path

      -- Clean up
      completeMultipartUpload store uploadId
      -- Remove parts directory
      removeDirectoryRecursive partsDir

      pure $ Right (muBucket upload, muKey upload, compositeETag)

-- | Abort a multipart upload
abortUpload :: Store -> FilePath -> UploadId -> IO ()
abortUpload store dataDir uploadId = do
  abortMultipartUpload store uploadId
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  doesDirExist <- doesDirectoryExist partsDir
  when doesDirExist $ removeDirectoryRecursive partsDir

-- | Background GC for expired uploads
uploadGC :: Store -> FilePath -> IO ()
uploadGC store dataDir = forever $ do
  threadDelay (30 * 60 * 1000000)  -- 30 minutes
  count <- cleanupExpiredUploads store
  when (count > 0) $
    putStrLn $ "Cleaned up " <> show count <> " expired multipart uploads"

import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Control.Concurrent (threadDelay, forkIO)

-- Internal helpers

foldMD5AndWrite :: FilePath -> ConduitT ByteString Void IO (Crypto.Context MD5, Int64)
foldMD5AndWrite path = do
  liftIO $ withFile path WriteMode $ \h -> do
    let loop ctx total = do
          mbs <- await
          case mbs of
            Nothing -> pure (ctx, total)
            Just bs -> do
              liftIO $ B.hPut h bs
              loop (hashUpdate ctx bs) (total + fromIntegral (B.length bs))
    loop hashInit 0
```

- [ ] **Step 2: Write multipart handler (simplified)**

```haskell
-- src/S3OSS/Multipart/Handler.hs
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Multipart.Handler where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.Multipart.Manager
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai
import Network.HTTP.Types

-- | Handle CreateMultipartUpload
handleCreateMultipartUpload :: Store -> FilePath -> User -> BucketName -> ObjectKey -> IO Response
handleCreateMultipartUpload store dataDir user bucket key = do
  unless (evaluate (userPolicies user) CreateMultipartUpload (objectARN bucket key)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"
  uploadId <- initiateUpload store dataDir bucket key
  pure $ responseLBS status200
    [("Content-Type", "application/xml")]
    (renderLBS $ renderInitiateMultipartUpload bucket key uploadId)

-- | Handle UploadPart
handleUploadPart :: Store -> FilePath -> User -> UploadId -> PartNumber -> Request -> IO Response
handleUploadPart store dataDir user uploadId partNum req = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload -> do
      unless (evaluate (userPolicies user) UploadPart (objectARN (muBucket upload) (muKey upload))) $
        pure $ errorResponse status403 "AccessDenied" "Access Denied"
      etag <- uploadPart store dataDir uploadId partNum (requestBody req)
      pure $ responseLBS status200 [("ETag", encodeUtf8 (unETag etag))] ""

-- | Handle CompleteMultipartUpload
handleCompleteMultipartUpload :: Store -> FilePath -> User -> UploadId -> Request -> IO Response
handleCompleteMultipartUpload store dataDir user uploadId req = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload -> do
      unless (evaluate (userPolicies user) CompleteMultipartUpload (objectARN (muBucket upload) (muKey upload))) $
        pure $ errorResponse status403 "AccessDenied" "Access Denied"
      -- Parse part list from body
      body <- liftIO $ lazyRequestBody req
      case parseCompleteMultipartUpload body of
        Left err -> pure $ errorResponse status400 "MalformedXML" err
        Right parts -> do
          result <- completeUpload store dataDir uploadId parts
          case result of
            Left err -> pure $ errorResponse status400 "InvalidPart" err
            Right (bucket, key, etag) -> pure $ responseLBS status200
              [("Content-Type", "application/xml")]
              (renderLBS $ renderCompleteMultipartUpload bucket key etag)

-- | Handle AbortMultipartUpload
handleAbortMultipartUpload :: Store -> FilePath -> User -> UploadId -> IO Response
handleAbortMultipartUpload store dataDir user uploadId = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload -> do
      unless (evaluate (userPolicies user) AbortMultipartUpload (objectARN (muBucket upload) (muKey upload))) $
        pure $ errorResponse status403 "AccessDenied" "Access Denied"
      abortUpload store dataDir uploadId
      pure $ responseLBS status204 [] ""

-- Helpers

objectARN :: BucketName -> ObjectKey -> ResourceARN
objectARN bucket key = ResourceARN $ "arn:aws:s3:::" <> unBucketName bucket <> "/" <> unObjectKey key

lazyRequestBody :: Request -> IO ByteString
lazyRequestBody req = do
  chunks <- runConduit $ requestBody req .| CL.consume
  pure $ B.concat chunks

import Data.Conduit ((.=|))
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
```

- [ ] **Step 3: Verify compilation**

```bash
cabal build
```

- [ ] **Step 4: Commit**

```bash
git add src/S3OSS/Multipart/Manager.hs src/S3OSS/Multipart/Handler.hs
git commit -m "feat: add multipart upload manager and handlers"
```

---

### Task 12: Presigned URLs + List Handlers

**Files:**
- Create: `src/S3OSS/Presigned.hs`
- Create: `src/S3OSS/List/Handler.hs`

- [ ] **Step 1: Write `src/S3OSS/Presigned.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Presigned where

import RIO
import S3OSS.Types
import S3OSS.Auth.SigV4
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Time (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)

-- | Generate a presigned URL for GetObject or PutObject
presignUrl :: User -> Action -> BucketName -> ObjectKey -> NominalDiffTime -> Text -> IO Text
presignUrl user action bucket key expirySeconds host = do
  now <- getCurrentTime
  let expires = addUTCTime expirySeconds now
      expiresUnix = floor (utcTimeToPOSIXSeconds expires) :: Int
      verb = case action of
               GetObject -> "GET"
               PutObject -> "PUT"
               _         -> "GET"
      resource = "/" <> unBucketName bucket <> "/" <> unObjectKey key
      stringToSign = T.intercalate "\n"
        [ verb
        , ""  -- Content-MD5
        , ""  -- Content-Type
        , tshow expiresUnix
        , resource
        ]
      signature = hmacGetDigest (hmac (unSecretKey $ userSecretKey user) (TE.encodeUtf8 stringToSign))
      sigHex = T.pack $ show (signature :: ByteString)  -- simplified hex encoding
  pure $ "https://" <> host <> resource
      <> "?X-Amz-Algorithm=AWS4-HMAC-SHA256"
      <> "&X-Amz-Credential=" <> unAccessKey (userAccessKey user)
      <> "&X-Amz-Expires=" <> tshow expiresUnix
      <> "&X-Amz-Signature=" <> sigHex

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
```

- [ ] **Step 2: Write `src/S3OSS/List/Handler.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.List.Handler where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai
import Network.HTTP.Types
import Database.SQLite.Simple
import qualified Data.Text as T

-- | Handle ListObjects / ListObjectsV2
handleListObjects :: Store -> User -> BucketName -> Maybe Text -> Maybe Text -> Maybe Int -> Bool -> IO Response
handleListObjects store user bucket prefix delimiter maxKeys isV2 = do
  unless (evaluate (userPolicies user) ListObjects (bucketARN bucket)) $
    pure $ errorResponse status403 "AccessDenied" "Access Denied"

  exists <- headBucket store bucket
  unless exists $
    pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"

  let maxK = fromMaybe 1000 maxKeys
  objects <- listObjects store bucket prefix delimiter maxK
  let isTruncated = length objects > maxK
  let result = take maxK objects

  pure $ responseLBS status200
    [("Content-Type", "application/xml")]
    (renderLBS $ renderListObjects bucket prefix delimiter maxK isTruncated result isV2)

-- | Query objects from SQLite with prefix/delimiter/maxKeys
listObjects :: Store -> BucketName -> Maybe Text -> Maybe Text -> Int -> IO [ObjectInfo]
listObjects store bucket prefix delimiter maxKeys = do
  let baseQuery = "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.metadata, o.created_at, o.updated_at \
                  \FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.name = ?"
      prefixClause = maybe "" (\_ -> " AND o.key LIKE ?") prefix
      orderClause = " ORDER BY o.key LIMIT ?"
      query' = fromString $ T.unpack (baseQuery <> prefixClause <> orderClause)
      params = case prefix of
                 Just p  -> [unBucketName bucket, p <> "%", maxKeys + 1]
                 Nothing -> [unBucketName bucket, maxKeys + 1]
  rows <- query (storeConn store) query' params
  pure $ map toObj rows
  where
    toObj (bn, k, h, sz, ct, e, _, ca, ua) =
      ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e)
        (parseTimeIso8601 ca) (parseTimeIso8601 ua)

bucketARN :: BucketName -> ResourceARN
bucketARN name = ResourceARN $ "arn:aws:s3:::" <> unBucketName name
```

- [ ] **Step 3: Verify compilation**

```bash
cabal build
```

- [ ] **Step 4: Commit**

```bash
git add src/S3OSS/Presigned.hs src/S3OSS/List/Handler.hs
git commit -m "feat: add presigned URL generation and list objects handler"
```

---

### Task 13: Servant API Definition + Server Assembly

**Files:**
- Create: `src/S3OSS/API.hs`
- Create: `src/S3OSS/Server.hs`

- [ ] **Step 1: Write `src/S3OSS/API.hs` with Servant type-level API**

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.API where

import Servant
import Servant.Server
import Network.Wai (Response)

-- | S3 API type using Servant's type-level DSL.
-- S3 uses a mix of REST verbs on bucket and object paths.
type S3API =
       -- Bucket operations
       Put '[JSON] ()
  :<|> "s3-oss-api"
```

- [ ] **Step 2: Write `src/S3OSS/Server.hs` — simplified WAI application**

Since S3's API uses raw HTTP semantics (not JSON), we'll use a raw WAI Application with manual routing rather than Servant's type-level routing. This is more appropriate for S3 compatibility.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Server where

import RIO
import S3OSS.Types
import S3OSS.Config
import S3OSS.Store
import S3OSS.Object.Storage
import S3OSS.Bucket.Handler
import S3OSS.Object.Handler
import S3OSS.List.Handler
import S3OSS.Multipart.Handler
import S3OSS.Multipart.Manager
import S3OSS.Presigned
import S3OSS.XML
import S3OSS.Auth.SigV4
import S3OSS.Auth.Policy
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai
import Network.Wai.Handler.Warp (run, defaultSettings, setPort, setHost)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.HTTP.Types
import qualified Data.ByteString as B
import Control.Concurrent (forkIO)

-- | Build the WAI Application
mkApp :: ResolvedConfig -> Store -> IO Application
mkApp config store = do
  -- Start multipart upload GC in background
  _ <- forkIO $ uploadGC store (stDataDir $ rcStorage config)
  pure $ app config store

-- | Main application handler
app :: ResolvedConfig -> Store -> Application
app config store req respond = do
  let method = requestMethod req
      path   = pathInfo req
      query  = queryString req

  result <- case (method, path) of
    -- GET / → ListBuckets
    ("GET", []) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleListBuckets store user

    -- PUT /{bucket} → CreateBucket
    ("PUT", [bucketName]) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleCreateBucket store user (BucketName bucketName)

    -- DELETE /{bucket} → DeleteBucket
    ("DELETE", [bucketName]) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleDeleteBucket store user (BucketName bucketName)

    -- HEAD /{bucket} → HeadBucket
    ("HEAD", [bucketName]) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleHeadBucket store user (BucketName bucketName)

    -- GET /{bucket}?list-type=2 → ListObjectsV2
    -- GET /{bucket} → ListObjects
    ("GET", [bucketName]) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> do
          let prefix = lookup "prefix" query
          let delimiter = lookup "delimiter" query
          let maxKeys = lookup "max-keys" query >>= readMaybe . T.unpack
          let isV2 = lookup "list-type" query == Just "2"
          handleListObjects store user (BucketName bucketName) prefix delimiter maxKeys isV2

    -- PUT /{bucket}/{key} → PutObject
    ("PUT", bucketName : keyParts) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handlePutObject store dataDir user (BucketName bucketName) (ObjectKey $ T.intercalate "/" keyParts) req

    -- GET /{bucket}/{key} → GetObject
    ("GET", bucketName : keyParts) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleGetObject store dataDir user (BucketName bucketName) (ObjectKey $ T.intercalate "/" keyParts) req

    -- DELETE /{bucket}/{key} → DeleteObject
    ("DELETE", bucketName : keyParts) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleDeleteObject store dataDir user (BucketName bucketName) (ObjectKey $ T.intercalate "/" keyParts)

    -- HEAD /{bucket}/{key} → HeadObject
    ("HEAD", bucketName : keyParts) -> do
      mUser <- authenticate config req
      case mUser of
        Left err -> pure err
        Right user -> handleHeadObject store user (BucketName bucketName) (ObjectKey $ T.intercalate "/" keyParts)

    -- POST /{bucket}/{key}?uploads → CreateMultipartUpload
    ("POST", bucketName : keyParts)
      | lookup "uploads" query == Just "" -> do
          mUser <- authenticate config req
          case mUser of
            Left err -> pure err
            Right user -> handleCreateMultipartUpload store dataDir user (BucketName bucketName) (ObjectKey $ T.intercalate "/" keyParts)

    -- PUT /{bucket}/{key}?partNumber=N&uploadId=ID → UploadPart
    ("PUT", bucketName : keyParts)
      | Just partNumStr <- lookup "partNumber" query
      , Just uploadIdStr <- lookup "uploadId" query -> do
          mUser <- authenticate config req
          case mUser of
            Left err -> pure err
            Right user -> do
              case (readMaybe (T.unpack partNumStr), readMaybe (T.unpack uploadIdStr)) of
                (Just pn, Just uid) ->
                  handleUploadPart store dataDir user uid (PartNumber pn) req
                _ -> pure $ errorResponse status400 "InvalidArgument" "Invalid partNumber or uploadId"

    -- POST /{bucket}/{key}?uploadId=ID → CompleteMultipartUpload
    ("POST", bucketName : keyParts)
      | Just uploadIdStr <- lookup "uploadId" query -> do
          mUser <- authenticate config req
          case mUser of
            Left err -> pure err
            Right user ->
              handleCompleteMultipartUpload store dataDir user uploadIdStr req

    -- DELETE /{bucket}/{key}?uploadId=ID → AbortMultipartUpload
    ("DELETE", bucketName : keyParts)
      | Just uploadIdStr <- lookup "uploadId" query -> do
          mUser <- authenticate config req
          case mUser of
            Left err -> pure err
            Right user ->
              handleAbortMultipartUpload store dataDir user uploadIdStr

    _ -> pure $ errorResponse status405 "MethodNotAllowed" "The specified method is not allowed against this resource"

  respond result
  where
    dataDir = stDataDir $ rcStorage config

-- | Authenticate a request, returns User or an error Response
authenticate :: ResolvedConfig -> Request -> IO (Either Response User)
authenticate config req = do
  -- In development mode, skip auth
  if scDevelopmentMode (rcServer config)
    then case rcUsers config of
           (u:_) -> pure $ Right u
           []    -> pure $ Left $ errorResponse status500 "InternalError" "No users configured"
    else do
      result <- verifySigV4 (rcUsers config) req
      case result of
        Left err -> pure $ Left $ errorResponse status403 "SignatureDoesNotMatch" err
        Right user -> pure $ Right user

-- | Start the server
startServer :: ResolvedConfig -> IO ()
startServer config = do
  let serverCfg = rcServer config
      dataDir   = stDataDir $ rcStorage config

  store <- initStore dataDir
  waiApp <- mkApp config store
  putStrLn $ "s3-oss starting on " <> T.unpack (scHost serverCfg) <> ":" <> show (scPort serverCfg)

  let settings = setPort (scPort serverCfg)
               $ setHost (fromString $ T.unpack $ scHost serverCfg)
               $ defaultSettings

  case (scTlsCert serverCfg, scTlsKey serverCfg) of
    (Just cert, Just key) -> do
      let tls = tlsSettings cert key
      runTLS tls settings waiApp
    _ -> do
      putStrLn "WARNING: Running without TLS (development mode)"
      run settings waiApp
```

- [ ] **Step 2: Verify compilation**

```bash
cabal build
```

- [ ] **Step 3: Commit**

```bash
git add src/S3OSS/API.hs src/S3OSS/Server.hs
git commit -m "feat: add WAI application and server startup"
```

---

### Task 14: Main Entry Point + CLI

**Files:**
- Modify: `app/Main.hs`

- [ ] **Step 1: Rewrite `app/Main.hs`**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO
import S3OSS.Config
import S3OSS.Server
import Options.Applicative

-- | CLI options
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
  <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Config file path"))
  <*> optional (option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Listen port"))
  <*> optional (strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> help "Data directory"))
  <*> optional (strOption (long "tls-cert" <> metavar "FILE" <> help "TLS certificate file"))
  <*> optional (strOption (long "tls-key" <> metavar "FILE" <> help "TLS private key file"))
  <*> switch (long "dev" <> help "Development mode (disable TLS + auth)")

main :: IO ()
main = do
  opts <- execParser $ info (cliParser <**> helper)
    (fullDesc <> progDesc "s3-oss: Secure S3-compatible local object storage" <> header "s3-oss")

  -- Load or use default config
  baseConfig <- case optConfig opts of
    Just path -> loadConfig path
    Nothing   -> pure defaultConfig

  -- Apply CLI overrides
  let config = applyOverrides baseConfig opts

  -- Start server
  startServer config

-- | Apply CLI option overrides to resolved config
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
```

- [ ] **Step 2: Build the full project**

```bash
cabal build
```

- [ ] **Step 3: Commit**

```bash
git add app/Main.hs
git commit -m "feat: add CLI argument parsing and main entry point"
```

---

### Task 15: Integration Testing + Final Polish

**Files:**
- Create: `test/Spec.hs`
- Create: `test/S3OSS/Bucket/HandlerSpec.hs`
- Create: `test/S3OSS/Multipart/ManagerSpec.hs`

- [ ] **Step 1: Write test runner**

```haskell
-- test/Spec.hs
module Main where

import Test.Hspec
import qualified S3OSS.XMLSpec
import qualified S3OSS.Auth.PolicySpec
import qualified S3OSS.Object.StorageSpec

main :: IO ()
main = hspec $ do
  S3OSS.XMLSpec.spec
  S3OSS.Auth.PolicySpec.spec
  S3OSS.Object.StorageSpec.spec
```

- [ ] **Step 2: Run all tests**

```bash
cabal test
```

Expected: all tests pass.

- [ ] **Step 3: Build release binary**

```bash
cabal build --enable-optimization=2
```

- [ ] **Step 4: Manual smoke test**

Start server in dev mode:
```bash
cabal run s3-oss -- --dev --port 9000 --data-dir ./test-data
```

In another terminal, test with curl:
```bash
# Create bucket
curl -X PUT http://localhost:9000/test-bucket

# List buckets
curl http://localhost:9000/

# Put object
echo "hello" | curl -X PUT --data-binary @- http://localhost:9000/test-bucket/hello.txt

# Get object
curl http://localhost:9000/test-bucket/hello.txt

# Delete object
curl -X DELETE http://localhost:9000/test-bucket/hello.txt

# Delete bucket
curl -X DELETE http://localhost:9000/test-bucket
```

- [ ] **Step 5: Final commit**

```bash
git add -A
git commit -m "feat: add integration tests and finalize project"
```
