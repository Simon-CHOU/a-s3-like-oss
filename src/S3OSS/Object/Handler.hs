{-# LANGUAGE OverloadedStrings #-}

-- | Object HTTP handlers.
module S3OSS.Object.Handler
  ( handlePutObject
  , handleGetObject
  , handleDeleteObject
  , handleHeadObject
  , handleCopyObject
  , objectARN
  , errorResponse
  ) where

import RIO hiding (evaluate)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy ()
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.List as CL
import Data.Time (getCurrentTime)
import Network.HTTP.Types (Status, status200, status204, status403, status404, status500, hContentType, hETag, hContentLength)
import Network.Wai (Response, responseLBS, responseStream)
import qualified RIO.Text as T
import System.Directory (doesFileExist)

import S3OSS.Auth.Policy
import S3OSS.Object.Storage
import S3OSS.Store
import S3OSS.Types
import S3OSS.XML

-- | Handle PutObject (PUT /{bucket}/{key}).
handlePutObject :: Store -> FilePath -> User -> BucketName -> ObjectKey -> ConduitT () ByteString IO () -> IO Response
handlePutObject store dataDir user bucket key source =
  if not (evaluate (userPolicies user) S3PutObject (objectARN bucket key))
    then pure $ errorResponse status403 "AccessDenied" "Access Denied"
    else do
      mBid <- getBucketIdMaybe store bucket
      case mBid of
        Nothing -> pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"
        Just _ -> do
          let contentType = Nothing
          (sha256, size, etag) <- putObject dataDir source
          result <- putObjectMeta store bucket key sha256 size contentType [] etag
          case result of
            Left _  -> pure $ errorResponse status404 "NoSuchBucket" "The specified bucket does not exist"
            Right _ -> pure $ responseLBS status200 [] ""

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
          let hashText = unSha256Hex (oiHash obj)
              prefix   = T.take 2 hashText
              filePath = dataDir <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack hashText
          fileExists <- doesFileExist filePath
          if not fileExists
            then pure $ errorResponse status500 "InternalError" "The object content was not found on the storage backend."
            else do
              let headers = [ (hContentType, fromMaybe "application/octet-stream" (encodeUtf8 <$> oiContentType obj))
                            , (hETag, encodeUtf8 (unETag (oiETag obj)))
                            , (hContentLength, fromString (show (oiSize obj)))
                            ]
              pure $ responseStream status200 headers $ \write flush -> do
                result <- try (runConduitRes $ getObject dataDir (oiHash obj) .| CL.mapM_ (\bs -> liftIO $ write (byteString bs))) :: IO (Either SomeException ())
                case result of
                  Left (_ :: SomeException) -> pure ()
                  Right _ -> flush

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
          deleted <- deleteObjectMeta store bucket key
          if deleted
            then do
              deleteObject dataDir (oiHash obj)
              pure $ responseLBS status204 [] ""
            else pure $ errorResponse status500 "InternalError" "Failed to delete object metadata"

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
      mBid <- getBucketIdMaybe store dstBucket
      case mBid of
        Nothing -> pure $ errorResponse status404 "NoSuchBucket" "The specified destination bucket does not exist"
        Just _ -> do
          mObj <- getObjectMeta store srcBucket srcKey
          case mObj of
            Nothing -> pure $ errorResponse status404 "NoSuchKey" "The specified key does not exist."
            Just obj -> do
              now <- getCurrentTime
              result <- putObjectMeta store dstBucket dstKey (oiHash obj) (oiSize obj)
                          (oiContentType obj) (oiMetadata obj) (oiETag obj)
              case result of
                Left _  -> pure $ errorResponse status404 "NoSuchBucket" "The specified destination bucket does not exist"
                Right _ -> pure $ responseLBS status200
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
