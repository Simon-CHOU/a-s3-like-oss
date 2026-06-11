{-# LANGUAGE OverloadedStrings #-}

-- | Object HTTP handlers.
module S3OSS.Object.Handler where

import RIO hiding (evaluate)
import S3OSS.Types
import S3OSS.Store
import S3OSS.Object.Storage
import S3OSS.XML
import S3OSS.Auth.Policy
import Network.Wai (Response, responseLBS, responseStream)
import Network.HTTP.Types (Status, status200, status204, status403, status404)
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time (getCurrentTime)
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types.Header (hContentType, hETag, hContentLength)

-- | Handle PutObject (PUT /{bucket}/{key}).
handlePutObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> ConduitT () ByteString IO () -> IO Response
handlePutObject store dataDir user bucket key source =
  if not (evaluate (userPolicies user) S3PutObject (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      let contentType = Nothing
      (sha256, size, etag) <- putObject dataDir source
      _ <- putObjectMeta store bucket key sha256 size contentType [] etag
      pure $ responseLBS status200 [] ""

-- | Handle GetObject (GET /{bucket}/{key}).
handleGetObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> IO Response
handleGetObject store dataDir user bucket key =
  if not (evaluate (userPolicies user) S3GetObject (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      mObj <- getObjectMeta store bucket key
      case mObj of
        Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
        Just obj -> do
          let headers = [ (hContentType, fromMaybe "application/octet-stream" (encodeUtf8 <$> oiContentType obj))
                        , (hETag, encodeUtf8 (unETag (oiETag obj)))
                        , (hContentLength, fromString (show (oiSize obj)))
                        ]
          pure $ responseStream status200 headers $ \write flush -> do
            runConduitRes $ getObject dataDir (oiHash obj) .| CL.mapM_ (\bs -> liftIO $ write (byteString bs))
            flush

-- | Handle DeleteObject (DELETE /{bucket}/{key}).
handleDeleteObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> IO Response
handleDeleteObject store dataDir user bucket key =
  if not (evaluate (userPolicies user) S3DeleteObject (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      mObj <- getObjectMeta store bucket key
      case mObj of
        Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
        Just obj -> do
          _ <- deleteObjectMeta store bucket key
          deleteObject dataDir (oiHash obj)
          pure $ responseLBS status204 [] ""

-- | Handle HeadObject (HEAD /{bucket}/{key}).
handleHeadObject :: Store -> User -> BucketName -> ObjectKey -> IO Response
handleHeadObject store user bucket key =
  if not (evaluate (userPolicies user) S3HeadObject (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      mObj <- getObjectMeta store bucket key
      case mObj of
        Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
        Just obj -> pure $ responseLBS status200
          [ (hContentType, fromMaybe "application/octet-stream" (encodeUtf8 <$> oiContentType obj))
          , (hETag, encodeUtf8 (unETag (oiETag obj)))
          , (hContentLength, fromString (show (oiSize obj)))
          ] ""

-- | Handle CopyObject.
handleCopyObject :: Store -> User -> BucketName -> ObjectKey -> BucketName -> ObjectKey -> IO Response
handleCopyObject store user srcBucket srcKey dstBucket dstKey =
  if not (evaluate (userPolicies user) S3GetObject (objectARN srcBucket srcKey))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else if not (evaluate (userPolicies user) S3PutObject (objectARN dstBucket dstKey))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      mObj <- getObjectMeta store srcBucket srcKey
      case mObj of
        Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
        Just obj -> do
          now <- getCurrentTime
          _ <- putObjectMeta store dstBucket dstKey (oiHash obj) (oiSize obj)
                (oiContentType obj) (oiMetadata obj) (oiETag obj)
          pure $ responseLBS status200
            [("Content-Type", "application/xml")]
            (renderLBS $ renderCopyObjectResult (oiETag obj) now)

-- Helpers

objectARN :: BucketName -> ObjectKey -> ResourceARN
objectARN bucket key = ResourceARN $ "arn:aws:s3:::" <> unBucketName bucket <> "/" <> unObjectKey key

errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)
