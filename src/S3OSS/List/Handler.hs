{-# LANGUAGE OverloadedStrings #-}

-- | List objects handler.
module S3OSS.List.Handler (handleListObjects) where

import RIO hiding (evaluate)
import S3OSS.Types
import S3OSS.Store
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (Status, status200, status403, status404)

-- | Handle ListObjects (GET /{bucket}).
handleListObjects :: Store -> User -> BucketName -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> IO Response
handleListObjects store user bucket prefix delimiter marker maxKeys =
  if not (evaluate (userPolicies user) S3ListObjects (bucketARN bucket))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      exists <- headBucket store bucket
      if not exists
        then pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"
        else do
          let maxK = max 0 $ min 1000 $ fromMaybe 1000 maxKeys
          (objects, prefixes) <- listObjects store bucket prefix delimiter marker maxK
          let combinedCount = length objects + length prefixes
          let isTruncated = combinedCount > maxK
          let finalObjects = take maxK objects
          let finalPrefixes = take (maxK - length finalObjects) prefixes
          pure $ responseLBS status200
            [("Content-Type", "application/xml")]
            (renderLBS $ renderListObjects bucket prefix delimiter marker maxK isTruncated finalObjects finalPrefixes)

-- Helpers

bucketARN :: BucketName -> ResourceARN
bucketARN name = ResourceARN $ "arn:aws:s3:::" <> unBucketName name

errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)
