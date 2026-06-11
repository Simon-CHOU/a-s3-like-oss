{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core domain types for the S3-like OSS service.
module S3OSS.Types where

import RIO
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

-- | SHA-256 hash in hex encoding.
newtype Sha256Hex = Sha256Hex { unSha256Hex :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)

-- | ETag as used in S3 responses (quoted hex).
newtype ETag = ETag { unETag :: Text }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | Bucket name: 3-63 chars, lowercase, numbers, hyphens, dots.
newtype BucketName = BucketName { unBucketName :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)

-- | Object key: arbitrary Unicode string.
newtype ObjectKey = ObjectKey { unObjectKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)

-- | Access key identifier.
newtype AccessKey = AccessKey { unAccessKey :: Text }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | Secret access key (never logged or serialized).
newtype SecretKey = SecretKey { unSecretKey :: ByteString }

-- | Upload ID for multipart uploads.
newtype UploadId = UploadId { unUploadId :: Text }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | Part number in multipart upload (1-10000).
newtype PartNumber = PartNumber { unPartNumber :: Int }
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

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
