{-# LANGUAGE OverloadedStrings #-}

-- | Bucket HTTP handlers.
module S3OSS.Bucket.Handler
  ( handleCreateBucket
  , handleDeleteBucket
  , handleListBuckets
  , handleHeadBucket
  ) where

import RIO hiding (evaluate)
import S3OSS.Types
import S3OSS.Store
import S3OSS.XML
import Text.XML (Document(..), Prologue(..), Element(..), Name(..))
import S3OSS.Auth.Policy
import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (Status, status200, status204, status403, status404, status409)
import qualified Data.ByteString.Lazy as BL

-- | Handle CreateBucket (PUT /{bucket}).
handleCreateBucket :: Store -> User -> BucketName -> IO Response
handleCreateBucket store user name =
  if not (evaluate (userPolicies user) S3CreateBucket (bucketARN name))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      result <- createBucket store name
      case result of
        Right _  -> pure $ responseLBS status200
                      [("Content-Type", "application/xml")]
                      (renderCreateBucketResult name)
        Left _   -> pure $ errorResponse status409 "BucketAlreadyExists" "The requested bucket name is not available"

-- | Handle DeleteBucket (DELETE /{bucket}).
handleDeleteBucket :: Store -> User -> BucketName -> IO Response
handleDeleteBucket store user name =
  if not (evaluate (userPolicies user) S3DeleteBucket (bucketARN name))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      result <- deleteBucket store name
      case result of
        BucketDeleted  -> pure $ responseLBS status204 [] ""
        BucketNotEmpty -> pure $ errorResponse status409 "BucketNotEmpty" "The bucket you tried to delete is not empty"
        BucketNotFound -> pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"

-- | Handle ListBuckets (GET /).
handleListBuckets :: Store -> User -> IO Response
handleListBuckets store user =
  if not (evaluate (userPolicies user) S3ListBuckets (ResourceARN "*"))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      buckets <- listBuckets store
      let owner = OwnerInfo (userName user) (unAccessKey $ userAccessKey user)
      pure $ responseLBS status200
        [("Content-Type", "application/xml")]
        (renderLBS $ renderListBucketsResult owner buckets)

-- | Handle HeadBucket (HEAD /{bucket}).
handleHeadBucket :: Store -> User -> BucketName -> IO Response
handleHeadBucket store user name =
  if not (evaluate (userPolicies user) S3HeadBucket (bucketARN name))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      exists <- headBucket store name
      if exists
        then pure $ responseLBS status200 [] ""
        else pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"

-- Helpers

bucketARN :: BucketName -> ResourceARN
bucketARN name = ResourceARN $ "arn:aws:s3:::" <> unBucketName name

renderCreateBucketResult :: BucketName -> BL.ByteString
renderCreateBucketResult name =
  renderLBS $ Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "CreateBucketResult" Nothing Nothing) mempty
      [ elt "BucketName" [content $ unBucketName name] ]

errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)
