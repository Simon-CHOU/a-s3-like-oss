{-# LANGUAGE OverloadedStrings #-}

-- | Multipart upload state machine and GC.
module S3OSS.Multipart.Manager where

import RIO
import S3OSS.Types
import S3OSS.Store
import S3OSS.Object.Storage
import S3OSS.XML
import qualified RIO.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Data.Conduit (ConduitT, (.|), await, yield, transPipe, runConduitRes)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist, removeDirectoryRecursive, doesDirectoryExist, listDirectory)
import System.IO (IOMode(..), withFile)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (replicateM)
import Data.List (sort)
-- tryIO is re-exported from RIO via UnliftIO.Exception
import Crypto.Hash (MD5, Context, hashInit, hashUpdate, hashFinalize, SHA256)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Generate a unique upload ID.
generateUploadId :: IO UploadId
generateUploadId = do
  bytes <- replicateM 16 (randomRIO (0, 255) :: IO Int)
  let hexChars = concatMap (printf ("%02x" :: String)) bytes
  pure $ UploadId (T.pack hexChars)

-- | Initialize a multipart upload.
initiateUpload :: Store -> FilePath -> BucketName -> ObjectKey -> IO UploadId
initiateUpload store dataDir bucket key = do
  uploadId <- generateUploadId
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  createDirectoryIfMissing True partsDir
  _ <- createMultipartUpload store bucket key uploadId
  pure uploadId

-- | Upload a single part.
uploadPart :: Store -> FilePath -> UploadId -> PartNumber -> ConduitT () ByteString IO () -> IO ETag
uploadPart store dataDir uploadId partNum source = do
  let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)
  createDirectoryIfMissing True partsDir
  let partPath = partsDir <> "/part-" <> printf ("%05d" :: String) (unPartNumber partNum)

  -- Stream part to file, compute MD5 and size
  (md5Ctx, size) <- System.IO.withFile partPath WriteMode $ \h ->
    runConduitRes $ transPipe liftIO source .| transPipe liftIO (foldMD5AndWrite h)

  let md5Digest = hashFinalize md5Ctx
  let md5Text = T.pack $ show md5Digest
  let sha256Hex = Sha256Hex md5Text  -- simplified: use MD5 as content hash for parts
  let etag = ETag $ "\"" <> md5Text <> "\""

  -- Record part in DB
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
          -- Assemble parts into final object
          let partsDir = dataDir <> "/multipart/" <> T.unpack (unUploadId uploadId)

          -- Concatenate parts via conduit
          (sha256Hex, totalSize, etag) <- assembleObject dataDir partsDir storedParts

          -- Store metadata
          _ <- putObjectMeta store (muBucket upload) (muKey upload) sha256Hex totalSize Nothing [] etag

          -- Mark complete in DB and cleanup
          completeMultipartUpload store uploadId
          removeDirectoryRecursive partsDir

          pure $ Right (muBucket upload, muKey upload, etag)

-- | Assemble uploaded parts into the final object.
assembleObject :: FilePath -> FilePath -> [PartInfo] -> IO (Sha256Hex, Int64, ETag)
assembleObject dataDir partsDir parts = do
  -- Build a source that concatenates all part files in order
  let loadPart p = do
        let path = partsDir <> "/part-" <> printf ("%05d" :: String) (unPartNumber (piPartNumber p))
        bs <- liftIO $ B.readFile path
        yield bs
      sources = map loadPart parts
  -- Write to a temp file, computing hashes
  putObject dataDir (sequenceSources sources)

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
  Control.Concurrent.threadDelay (30 * 60 * 1000000)  -- 30 minutes
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

-- Internal: fold conduit bytes into MD5 context while writing to a file.
foldMD5AndWrite :: Handle -> ConduitT ByteString Void IO (Context MD5, Int64)
foldMD5AndWrite h = loop hashInit 0
  where
    loop ctx total = do
      mbs <- await
      case mbs of
        Nothing -> pure (ctx, total)
        Just bs -> do
          liftIO $ B.hPut h bs
          loop (hashUpdate ctx bs) (total + fromIntegral (B.length bs))
