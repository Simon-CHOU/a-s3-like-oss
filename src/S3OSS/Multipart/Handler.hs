{-# LANGUAGE OverloadedStrings #-}

-- | Multipart upload HTTP handlers.
module S3OSS.Multipart.Handler where

import RIO hiding (evaluate)
import S3OSS.Types
import S3OSS.Store
import S3OSS.Multipart.Manager
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (Status, status200, status204, status400, status403, status404)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT)

-- | Handle CreateMultipartUpload.
handleCreateMultipartUpload :: Store -> FilePath -> User -> BucketName -> ObjectKey -> IO Response
handleCreateMultipartUpload store dataDir user bucket key =
  if not (evaluate (userPolicies user) S3CreateMultipartUpload (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      result <- initiateUpload store dataDir bucket key
      case result of
        Left _ -> pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"
        Right uploadId ->
          pure $ responseLBS status200
            [("Content-Type", "application/xml")]
            (renderLBS $ renderInitiateMultipartUpload bucket key uploadId)

-- | Handle UploadPart.
handleUploadPart :: Store -> FilePath -> User -> UploadId -> PartNumber -> ConduitT () ByteString IO () -> IO Response
handleUploadPart store dataDir user uploadId partNum source = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload ->
      if not (evaluate (userPolicies user) S3UploadPart (objectARN (muBucket upload) (muKey upload)))
        then pure $ errorResponse status403 "AccessDenied" "Access Denied"
        else do
          etag <- uploadPart store dataDir uploadId partNum source
          pure $ responseLBS status200 [("ETag", encodeUtf8 (unETag etag))] ""

-- | Handle CompleteMultipartUpload.
handleCompleteMultipartUpload :: Store -> FilePath -> User -> UploadId -> BL.ByteString -> IO Response
handleCompleteMultipartUpload store dataDir user uploadId body = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload ->
      if not (evaluate (userPolicies user) S3CompleteMultipartUpload (objectARN (muBucket upload) (muKey upload)))
        then pure $ errorResponse status403 "AccessDenied" "Access Denied"
        else case parseCompleteMultipartUpload body of
          Left err -> pure $ errorResponse status400 "MalformedXML" err
          Right parts -> do
            result <- completeUpload store dataDir uploadId parts
            case result of
              Left err -> pure $ errorResponse status400 "InvalidPart" err
              Right (bucket, key, etag) ->
                pure $ responseLBS status200
                  [("Content-Type", "application/xml")]
                  (renderLBS $ renderCompleteMultipartUpload bucket key etag)

-- | Handle AbortMultipartUpload.
handleAbortMultipartUpload :: Store -> FilePath -> User -> UploadId -> IO Response
handleAbortMultipartUpload store dataDir user uploadId = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ errorResponse status404 "NoSuchUpload" "The specified upload does not exist"
    Just upload ->
      if not (evaluate (userPolicies user) S3AbortMultipartUpload (objectARN (muBucket upload) (muKey upload)))
        then pure $ errorResponse status403 "AccessDenied" "Access Denied"
        else do
          abortUpload store dataDir uploadId
          pure $ responseLBS status204 [] ""

-- Helpers

objectARN :: BucketName -> ObjectKey -> ResourceARN
objectARN bucket key = ResourceARN $ "arn:aws:s3:::" <> unBucketName bucket <> "/" <> unObjectKey key

errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)
