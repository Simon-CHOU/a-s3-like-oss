{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core domain types for the S3-like OSS service.
module S3OSS.Types (module S3OSS.Types) where

import RIO
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Char (isLower, isDigit)
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

-- | SHA-256 hash in hex encoding.
newtype Sha256Hex = Sha256Hex { unSha256Hex :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for Sha256Hex.
-- Validates: exactly 64 lowercase hex characters.
mkSha256Hex :: Text -> Either Text Sha256Hex
mkSha256Hex t
  | T.length t /= 64 = Left "SHA-256 hash must be exactly 64 hex characters"
  | not (T.all isLowerHex t) = Left "SHA-256 hash must contain only lowercase hex digits"
  | otherwise = Right (Sha256Hex t)
  where
    isLowerHex c = isDigit c || (c >= 'a' && c <= 'f')

-- | ETag as used in S3 responses (quoted hex).
newtype ETag = ETag { unETag :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Bucket name: 3-63 chars, lowercase, numbers, hyphens, dots.
newtype BucketName = BucketName { unBucketName :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for BucketName.
-- Validates: 3-63 chars, lowercase letters, numbers, hyphens, dots;
-- must start and end with alphanumeric; no consecutive dots; not IP-style.
mkBucketName :: Text -> Either Text BucketName
mkBucketName t
  | T.length t < 3 || T.length t > 63 =
      Left "Bucket name must be between 3 and 63 characters"
  | not (T.all isValidBucketChar t) =
      Left "Bucket name contains invalid characters"
  | T.head t == '.' = Left "Bucket name must not start with a dot"
  | T.last t == '.' = Left "Bucket name must not end with a dot"
  | T.head t == '-' = Left "Bucket name must not start with a hyphen"
  | T.last t == '-' = Left "Bucket name must not end with a hyphen"
  | isDigit (T.head t) = Left "Bucket name must not start with a number"
  | ".." `T.isInfixOf` t = Left "Bucket name must not contain consecutive dots"
  | let segments = T.split (== '.') t
  , all (\s -> not (T.null s) && T.all isDigit s) segments
  = Left "Bucket name must not be formatted as an IP address"
  | otherwise = Right (BucketName t)

isValidBucketChar :: Char -> Bool
isValidBucketChar c = isLower c || isDigit c || c == '-' || c == '.'

instance ToField BucketName where toField = toField . unBucketName
instance FromField BucketName where fromField f = BucketName <$> fromField f

-- | Object key: arbitrary Unicode string (max 1024 bytes in UTF-8).
newtype ObjectKey = ObjectKey { unObjectKey :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for ObjectKey.
-- Validates: non-empty, max 1024 UTF-8 bytes.
mkObjectKey :: Text -> Either Text ObjectKey
mkObjectKey t
  | T.null t = Left "Object key must not be empty"
  | BS.length (encodeUtf8 t) > 1024 = Left "Object key must be at most 1024 bytes"
  | otherwise = Right (ObjectKey t)

instance ToField ObjectKey where toField = toField . unObjectKey
instance FromField ObjectKey where fromField f = ObjectKey <$> fromField f

-- | Access key identifier.
newtype AccessKey = AccessKey { unAccessKey :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Secret access key (never logged or serialized).
newtype SecretKey = SecretKey { unSecretKey :: ByteString }
  deriving (Eq)

instance Show SecretKey where
  show _ = "SecretKey {unSecretKey = <secret>}"

-- | Upload ID for multipart uploads.
newtype UploadId = UploadId { unUploadId :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Part number in multipart upload (1-10000).
newtype PartNumber = PartNumber { unPartNumber :: Int }
  deriving stock (Show, Eq, Ord, Generic)

-- | Smart constructor for PartNumber.
-- Validates: between 1 and 10000 inclusive.
mkPartNumber :: Int -> Either Text PartNumber
mkPartNumber n
  | n < 1 || n > 10000 = Left "Part number must be between 1 and 10000"
  | otherwise = Right (PartNumber n)

-- | S3 action for authorization.
data Action
  = S3GetObject
  | S3PutObject
  | S3DeleteObject
  | S3HeadObject
  | S3CopyObject
  | S3ListObjects
  | S3CreateBucket
  | S3DeleteBucket
  | S3ListBuckets
  | S3HeadBucket
  | S3CreateMultipartUpload
  | S3UploadPart
  | S3CompleteMultipartUpload
  | S3AbortMultipartUpload
  | S3AllActions
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Policy effect.
data Effect = Allow | Deny
  deriving (Show, Eq, Generic)

-- | Resource ARN pattern.
newtype ResourceARN = ResourceARN { unResourceARN :: Text }
  deriving (Show, Eq, Generic)

-- | IAM-like policy statement.
data Policy = Policy
  { policyEffect    :: Effect
  , policyActions   :: [Action]
  , policyResources :: [ResourceARN]
  }
  deriving (Show, Eq, Generic)

-- | User with credentials and policies.
data User = User
  { userName      :: Text
  , userAccessKey :: AccessKey
  , userSecretKey :: SecretKey
  , userPolicies  :: [Policy]
  }
  deriving (Show, Eq)

-- | Object metadata.
data ObjectInfo = ObjectInfo
  { oiBucket      :: BucketName
  , oiKey         :: ObjectKey
  , oiHash        :: Sha256Hex
  , oiSize        :: Int64
  , oiContentType :: Maybe Text
  , oiMetadata    :: [(Text, Text)]
  , oiETag        :: ETag
  , oiCreatedAt   :: UTCTime
  , oiUpdatedAt   :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Bucket metadata.
data BucketInfo = BucketInfo
  { biName      :: BucketName
  , biCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Multipart upload state.
data UploadState
  = UploadInitiated
  | UploadInProgress
  | UploadCompleted
  | UploadAborted
  deriving (Show, Eq, Generic)

-- | Multipart upload record.
data MultipartUpload = MultipartUpload
  { muUploadId  :: UploadId
  , muBucket    :: BucketName
  , muKey       :: ObjectKey
  , muState     :: UploadState
  , muCreatedAt :: UTCTime
  , muExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | A completed part in a multipart upload.
data PartInfo = PartInfo
  { piPartNumber :: PartNumber
  , piHash       :: Sha256Hex
  , piSize       :: Int64
  , piETag       :: ETag
  }
  deriving (Show, Eq, Generic)

-- | Owner info for XML responses.
data OwnerInfo = OwnerInfo
  { ownerDisplayName :: Text
  , ownerId          :: Text
  }
  deriving (Show, Eq, Generic)
