{-# LANGUAGE OverloadedStrings #-}

-- | Multipart upload state machine and GC.
module S3OSS.Multipart.Manager where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.Object.Storage
import qualified RIO.Text as T
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, (.|), await, yield, transPipe, runConduitRes)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist, listDirectory)
import qualified System.IO
import Control.Monad (replicateM)
import Data.List (sort)
import Crypto.Hash (MD5, SHA256, Context, hashInit, hashUpdate, hashFinalize)
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Generate a unique upload ID.
generateUploadId :: IO UploadId
generateUploadId = do
  bytes <- replicateM 16 (randomRIO (0, 255) :: IO Int)
  let hexChars = concatMap (printf ("%02x" :: String)) bytes
  pure $ UploadId (T.pack hexChars)

-- | Initialize a multipart upload.
initiateUpload :: Store -> FilePath -> BucketName -> ObjectKey -> IO (Either Text UploadId)
initiateUpload store dataDir bucket key = do
  uploadId <- generateUploadId
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  createDirectoryIfMissing True partsDir
  result <- createMultipartUpload store bucket key uploadId
  case result of
    Left err -> pure $ Left err
    Right _  -> pure $ Right uploadId

-- | Upload a single part.
uploadPart :: Store -> FilePath -> UploadId -> PartNumber -> ConduitT () ByteString IO () -> IO ETag
uploadPart store dataDir uploadId partNum source = do
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  createDirectoryIfMissing True partsDir
  let partPath = partsDir <> "/part-" <> printf ("%05d" :: String) (unPartNumber partNum)

  -- Stream part to file, compute SHA-256 and MD5
  (sha256Ctx, md5Ctx, size) <- System.IO.withFile partPath WriteMode $ \h ->
    runConduitRes $ transPipe liftIO source .| transPipe liftIO (foldMD5AndWrite h)

  let sha256Digest = hashFinalize sha256Ctx
  let md5Digest = hashFinalize md5Ctx
  let sha256Hex = Sha256Hex $ T.pack $ show sha256Digest
  let md5Text = T.pack $ show md5Digest
  let etag = ETag $ "\"" <> md5Text <> "\""

  -- Record part in DB with genuine SHA-256 hash
  addPart store uploadId partNum sha256Hex size etag
  pure etag

-- | Complete a multipart upload by assembling parts into final object.
completeUpload :: Store -> FilePath -> UploadId -> [(PartNumber, ETag)] -> IO (Either Text (BucketName, ObjectKey, ETag))
completeUpload store dataDir uploadId parts = do
  mUpload <- getMultipartUpload store uploadId
  case mUpload of
    Nothing -> pure $ Left "NoSuchUpload"
    Just upload -> do
      storedParts <- getParts store uploadId
      let storedPartNums = map piPartNumber storedParts
      let givenPartNums = map fst parts
      if sort givenPartNums /= sort storedPartNums
        then pure $ Left "InvalidPart"
        else do
          let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)

          -- Assemble parts into final object
          assembleResult <- assembleObject dataDir partsDir storedParts
          case assembleResult of
            Left err -> pure $ Left err
            Right (sha256Hex, totalSize, etag) -> do
              -- Wrap in exception handler to clean up on failure
              let work = do
                    putResult <- putObjectMeta store (muBucket upload) (muKey upload) sha256Hex totalSize Nothing [] etag
                    case putResult of
                      Left err -> pure $ Left err
                      Right _ -> do
                        completeMultipartUpload store uploadId
                        removeDirectoryRecursive partsDir
                        pure $ Right (muBucket upload, muKey upload, etag)
              result <- tryAny work
              case result of
                Left ex -> do
                  _ <- tryAny $ abortMultipartUpload store uploadId
                  _ <- tryAny $ removeDirectoryRecursive partsDir
                  pure $ Left $ T.pack (show ex)
                Right (Left err) -> pure $ Left err
                Right (Right val) -> pure $ Right val

-- | Assemble uploaded parts into the final object.
-- Streams part files from disk in 64 KB chunks to avoid loading entire parts into memory.
-- A missing part file is caught by tryAny and returned as Left instead of crashing.
assembleObject :: FilePath -> FilePath -> [PartInfo] -> IO (Either Text (Sha256Hex, Int64, ETag))
assembleObject dataDir partsDir parts = do
  -- Build a source that concatenates all part files in order, streaming from disk
  let chunkSize = 65536
      loadPart p = do
        let path = partsDir <> "/part-" <> printf ("%05d" :: String) (unPartNumber (piPartNumber p))
        h <- liftIO $ openFile path ReadMode
        let loop = do
              chunk <- liftIO $ B.hGetSome h chunkSize
              if B.null chunk
                then liftIO $ hClose h
                else yield chunk >> loop
        loop
      sources = map loadPart parts
  result <- tryAny $ putObject dataDir (sequenceSources sources)
  pure $ case result of
    Left ex -> Left $ T.pack (show ex)
    Right val -> Right val

-- | Create a conduit source that yields bytes from multiple sources in sequence.
sequenceSources :: [ConduitT () ByteString IO ()] -> ConduitT () ByteString IO ()
sequenceSources [] = pure ()
sequenceSources (src:rest) = src >> sequenceSources rest

-- | Abort a multipart upload.
abortUpload :: Store -> FilePath -> UploadId -> IO ()
abortUpload store dataDir uploadId = do
  abortMultipartUpload store uploadId
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  exists <- doesDirectoryExist partsDir
  when exists $ removeDirectoryRecursive partsDir

-- | Background GC for expired uploads.
uploadGC :: Store -> FilePath -> IO ()
uploadGC store dataDir = forever $ do
  threadDelay (30 * 60 * 1000000)  -- 30 minutes
  result <- tryAny $ do
    count <- cleanupExpiredUploads store
    when (count > 0) $
      putStrLn $ "Cleaned up " <> show count <> " expired multipart uploads"
    -- Also clean up orphaned multipart directories
    let multipartDir = dataDir <> "/multipart"
    mEntries <- tryIO (listDirectory multipartDir)
    case mEntries of
      Left _  -> pure ()
      Right entries -> do
        forM_ entries $ \uidDir -> do
          let uploadId = UploadId (T.pack uidDir)
          mUpload <- getMultipartUpload store uploadId
          case mUpload of
            Nothing -> do
              removeDirectoryRecursive (multipartDir <> "/" <> uidDir)
              putStrLn $ "Cleaned up orphaned multipart directory: " <> uidDir
            Just _ -> pure ()
  case result of
    Left ex -> putStrLn $ "uploadGC error: " <> show ex
    Right _ -> pure ()

-- | Internal: fold conduit bytes into SHA-256 and MD5 contexts while writing to a file.
foldMD5AndWrite :: Handle -> ConduitT ByteString Void IO (Context SHA256, Context MD5, Int64)
foldMD5AndWrite h = loop hashInit hashInit 0
  where
    loop shaCtx md5Ctx total = do
      mbs <- await
      case mbs of
        Nothing -> pure (shaCtx, md5Ctx, total)
        Just bs -> do
          liftIO $ B.hPut h bs
          let shaCtx' = hashUpdate shaCtx bs
          let md5Ctx' = hashUpdate md5Ctx bs
          loop shaCtx' md5Ctx' (total + fromIntegral (B.length bs))
