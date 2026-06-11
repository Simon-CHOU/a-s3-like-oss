{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Multipart.ManagerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import S3OSS.Multipart.Manager
import S3OSS.Store
import S3OSS.Types
import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import Data.Conduit ((.|), yield, runConduit)
import Data.Time (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Data.List (nub)
import Data.Char (isHexDigit)
import Data.Maybe (isJust)
import Control.Monad (replicateM)
import System.IO.Temp (withSystemTempDirectory)
import Database.SQLite.Simple (execute)

spec :: Spec
spec = do

  describe "generateUploadId" $ do
    it "produces 32-character hex strings" $ ioProperty $ do
      uid <- generateUploadId
      let hex = unUploadId uid
      pure $ counterexample ("got: " <> T.unpack hex) $
        T.length hex === 32 .&&. T.all isHexDigit hex

    it "generates unique IDs on successive calls" $ ioProperty $ do
      ids <- replicateM 20 generateUploadId
      pure $ length (nub ids) === 20

  describe "Upload lifecycle" $ do
    it "initiates an upload and persists it in the store" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        result <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        case result of
          Left err -> expectationFailure (T.unpack err)
          Right uid -> do
            T.length (unUploadId uid) `shouldSatisfy` (> 0)
            mUpload <- getMultipartUpload store uid
            isJust mUpload `shouldBe` True

    it "uploads parts and returns valid ETags" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        etag <- uploadPart store tmp uid (PartNumber 1) (yield ("hello" :: B.ByteString))
        T.length (unETag etag) `shouldSatisfy` (> 0)
        T.head (unETag etag) `shouldBe` '"'

    it "completes a multipart upload with multiple parts" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        e1 <- uploadPart store tmp uid (PartNumber 1) (yield ("part1" :: B.ByteString))
        e2 <- uploadPart store tmp uid (PartNumber 2) (yield ("part2" :: B.ByteString))
        result <- completeUpload store tmp uid [(PartNumber 1, e1), (PartNumber 2, e2)]
        case result of
          Left err -> expectationFailure (T.unpack err)
          Right (b, k, _) -> do
            b `shouldBe` BucketName "bucket"
            k `shouldBe` ObjectKey "key"
            -- The object should now exist in the store
            mObj <- getObjectMeta store b k
            isJust mObj `shouldBe` True

  describe "Abort" $ do
    it "cleans up upload metadata and uploaded parts" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        _ <- uploadPart store tmp uid (PartNumber 1) (yield ("data" :: B.ByteString))
        abortUpload store tmp uid
        mUpload <- getMultipartUpload store uid
        mUpload `shouldBe` Nothing
        parts <- getParts store uid
        parts `shouldBe` []

  describe "Part number uniqueness" $ do
    it "rejects complete request with duplicate part numbers" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        e1 <- uploadPart store tmp uid (PartNumber 1) (yield ("part1" :: B.ByteString))
        e2 <- uploadPart store tmp uid (PartNumber 2) (yield ("part2" :: B.ByteString))
        result <- completeUpload store tmp uid
          [(PartNumber 1, e1), (PartNumber 2, e2), (PartNumber 2, e2)]
        result `shouldBe` Left "InvalidPart"

    it "allows re-uploading same part number and uses latest" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        _ <- uploadPart store tmp uid (PartNumber 1) (yield ("old" :: B.ByteString))
        e1 <- uploadPart store tmp uid (PartNumber 1) (yield ("new" :: B.ByteString))
        result <- completeUpload store tmp uid [(PartNumber 1, e1)]
        case result of
          Left err -> expectationFailure (T.unpack err)
          Right _ -> pure ()

  describe "Cleanup of expired uploads" $ do
    it "removes expired uploads via cleanupExpiredUploads" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        pastTime <- addUTCTime (-86400) <$> getCurrentTime
        let pastIso = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" pastTime
        execute (storeConn store)
          "UPDATE multipart_uploads SET expires_at = ? WHERE upload_id = ?"
          (pastIso, unUploadId uid)
        count <- cleanupExpiredUploads store
        count `shouldBe` 1
        mUpload <- getMultipartUpload store uid
        mUpload `shouldBe` Nothing

    it "does not remove non-expired uploads" $
      withSystemTempDirectory "s3oss-mp-test" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "bucket")
        Right uid <- initiateUpload store tmp (BucketName "bucket") (ObjectKey "key")
        count <- cleanupExpiredUploads store
        count `shouldBe` 0
        mUpload <- getMultipartUpload store uid
        isJust mUpload `shouldBe` True

  describe "sequenceSources" $ do
    it "concatenates conduit sources in order" $ ioProperty $ do
      let src1 = yield ("abc" :: B.ByteString)
      let src2 = yield ("def" :: B.ByteString)
      result <- runConduit $ sequenceSources [src1, src2] .| CL.consume
      pure $ B.concat result === ("abcdef" :: B.ByteString)

    it "handles empty list of sources" $
      ioProperty $ do
        result <- runConduit $ sequenceSources [] .| CL.consume
        pure $ result === ([] :: [B.ByteString])
