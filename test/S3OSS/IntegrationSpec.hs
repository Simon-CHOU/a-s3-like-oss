{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for Store + Storage layers working together.
module S3OSS.IntegrationSpec (spec) where

import Test.Hspec
import S3OSS.Store
import S3OSS.Types
import S3OSS.Object.Storage (putObject, getObject, deleteObject)
import S3OSS.Multipart.Manager (initiateUpload, uploadPart, completeUpload, abortUpload)
import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import Data.Conduit ((.|), yield, runConduitRes)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import qualified Data.Text as T
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "Store + Storage Integration" $ do

    -----------------------------------------------------------
    -- (1) Create bucket, put object, get object, verify content
    -----------------------------------------------------------
    it "creates a bucket, puts an object, gets it back, and verifies content" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        -- Init store
        store <- initStore tmp
        let bucket = BucketName "test-bucket"
        let key    = ObjectKey "hello.txt"
        let content = "Hello, world!" :: B.ByteString

        -- Create bucket
        result <- createBucket store bucket
        result `shouldSatisfy` either (const False) (const True)

        -- Put object content to storage
        (sha256, size, etag) <- putObject tmp (yield content)
        T.length (unSha256Hex sha256) `shouldBe` 64
        size `shouldBe` fromIntegral (B.length content)

        -- Record object metadata in store
        metaResult <- putObjectMeta store bucket key sha256 size Nothing [] etag
        metaResult `shouldSatisfy` either (const False) (const True)

        -- Get object metadata back
        Just objMeta <- getObjectMeta store bucket key
        oiBucket objMeta `shouldBe` bucket
        oiKey objMeta `shouldBe` key
        oiHash objMeta `shouldBe` sha256
        oiSize objMeta `shouldBe` size

        -- Get object content back
        storedBytes <- runConduitRes $ getObject tmp sha256 .| CL.consume
        B.concat storedBytes `shouldBe` content

    -----------------------------------------------------------
    -- (2) Put same content twice, verify dedup (same SHA-256 file path)
    -----------------------------------------------------------
    it "deduplicates identical content (same SHA-256, single file on disk)" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        _ <- createBucket store (BucketName "dedup-bucket")
        let key    = ObjectKey "dedup.txt"
        let content = "deduplicate-me" :: B.ByteString

        -- First write
        (sha1, size1, etag1) <- putObject tmp (yield content)
        metaResult1 <- putObjectMeta store (BucketName "dedup-bucket") key sha1 size1 Nothing [] etag1
        metaResult1 `shouldSatisfy` either (const False) (const True)

        -- Second write with identical content
        (sha2, size2, etag2) <- putObject tmp (yield content)
        metaResult2 <- putObjectMeta store (BucketName "dedup-bucket") key sha2 size2 Nothing [] etag2
        metaResult2 `shouldSatisfy` either (const False) (const True)

        -- SHA-256 hashes must be identical
        sha1 `shouldBe` sha2
        size1 `shouldBe` size2

        -- Only one file exists in the content-addressed shard directory
        let sha256Text = unSha256Hex sha1
        let prefix = T.take 2 sha256Text
        let shardDir = tmp <> "/objects/" <> T.unpack prefix
        dirExists <- doesDirectoryExist shardDir
        dirExists `shouldBe` True
        shardFiles <- listDirectory shardDir
        filter (== T.unpack sha256Text) shardFiles `shouldBe` [T.unpack sha256Text]

    -----------------------------------------------------------
    -- (3) Delete object, verify file removed
    -----------------------------------------------------------
    it "deletes an object and removes its file from disk" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "delete-bucket"
        let key    = ObjectKey "delete-me.txt"
        let content = "delete me" :: B.ByteString

        -- Set up
        _ <- createBucket store bucket
        (sha256, size, etag) <- putObject tmp (yield content)
        _ <- putObjectMeta store bucket key sha256 size Nothing [] etag

        -- Verify it exists before deletion
        Just _ <- getObjectMeta store bucket key

        -- Delete from metadata store
        deleted <- deleteObjectMeta store bucket key
        deleted `shouldBe` True

        -- Delete from file storage
        deleteObject tmp sha256

        -- Verify object metadata is gone
        Nothing <- getObjectMeta store bucket key

        -- Verify file is gone from disk
        let sha256Text = unSha256Hex sha256
        let prefix = T.take 2 sha256Text
        let filePath = tmp <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack sha256Text
        fileExists <- doesFileExist filePath
        fileExists `shouldBe` False

    -----------------------------------------------------------
    -- (4) Delete all objects from bucket, delete bucket, verify bucket deleted
    -----------------------------------------------------------
    it "deletes all objects, then deletes the bucket, and verifies bucket is gone" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "cleanup-bucket"

        -- Create bucket
        _ <- createBucket store bucket

        -- Put multiple objects
        let objects = [(ObjectKey "alpha.txt", "content-alpha" :: B.ByteString),
                       (ObjectKey "beta.txt",  "content-beta"),
                       (ObjectKey "gamma.txt", "content-gamma")]
        forM_ objects $ \(key, content) -> do
          (sha, sz, et) <- putObject tmp (yield content)
          _ <- putObjectMeta store bucket key sha sz Nothing [] et
          pure ()

        -- Delete each object
        forM_ objects $ \(key, _) -> do
          Just objMeta <- getObjectMeta store bucket key
          deleted <- deleteObjectMeta store bucket key
          deleted `shouldBe` True
          deleteObject tmp (oiHash objMeta)

        -- Bucket should now be empty and deletable
        status <- deleteBucket store bucket
        status `shouldBe` BucketDeleted

        -- Verify bucket is gone
        exists <- headBucket store bucket
        exists `shouldBe` False

    -----------------------------------------------------------
    -- (5) Multipart upload: create, upload 3 parts, complete, verify content
    -----------------------------------------------------------
    it "completes a multipart upload with 3 parts and verifies assembled content" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "mp-bucket"
        let key    = ObjectKey "large-file.bin"

        -- Create bucket
        _ <- createBucket store bucket

        -- Initiate multipart upload
        Right uid <- initiateUpload store tmp bucket key

        -- Upload 3 parts
        let part1 = B.replicate (1024 * 64) 0x41  -- 64 KB of 'A'
        let part2 = B.replicate (1024 * 32) 0x42  -- 32 KB of 'B'
        let part3 = "final-chunk" :: B.ByteString

        etag1 <- uploadPart store tmp uid (PartNumber 1) (yield part1)
        etag2 <- uploadPart store tmp uid (PartNumber 2) (yield part2)
        etag3 <- uploadPart store tmp uid (PartNumber 3) (yield part3)

        -- Complete the upload
        result <- completeUpload store tmp uid
          [(PartNumber 1, etag1), (PartNumber 2, etag2), (PartNumber 3, etag3)]
        case result of
          Left err -> expectationFailure ("completeUpload failed: " <> T.unpack err)
          Right (b, k, _) -> do
            b `shouldBe` bucket
            k `shouldBe` key

        -- Get the assembled object metadata
        Just objMeta <- getObjectMeta store bucket key
        let expectedTotal = fromIntegral (B.length part1 + B.length part2 + B.length part3)
        oiSize objMeta `shouldBe` expectedTotal

        -- Verify assembled content matches
        storedBytes <- runConduitRes $ getObject tmp (oiHash objMeta) .| CL.consume
        let assembledContent = B.concat storedBytes
        assembledContent `shouldBe` part1 <> part2 <> part3

    -----------------------------------------------------------
    -- (6) Multipart abort: create, upload part, abort, verify parts deleted
    -----------------------------------------------------------
    it "aborts a multipart upload and cleans up parts" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "abort-bucket"
        let key    = ObjectKey "abort-me.bin"

        -- Create bucket
        _ <- createBucket store bucket

        -- Initiate multipart upload
        Right uid <- initiateUpload store tmp bucket key

        -- Upload a part
        _ <- uploadPart store tmp uid (PartNumber 1) (yield ("part-data" :: B.ByteString))

        -- Verify the part exists
        parts <- getParts store uid
        length parts `shouldBe` 1

        -- Verify the multipart directory exists
        let partsDir = tmp <> "/multipart/" <> T.unpack (unUploadId uid)
        dirExists <- doesDirectoryExist partsDir
        dirExists `shouldBe` True

        -- Abort the upload
        abortUpload store tmp uid

        -- Verify upload is gone from store
        mUpload <- getMultipartUpload store uid
        mUpload `shouldBe` Nothing

        -- Verify parts are gone from store
        partsAfter <- getParts store uid
        partsAfter `shouldBe` []

        -- Verify multipart directory is removed from disk
        dirExistsAfter <- doesDirectoryExist partsDir
        dirExistsAfter `shouldBe` False

    -----------------------------------------------------------
    -- (7) List objects with prefix and delimiter
    -----------------------------------------------------------
    it "lists objects with prefix and delimiter, returning common prefixes" $
      withSystemTempDirectory "s3oss-integration" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "list-bucket"
        _ <- createBucket store bucket

        -- Put objects with hierarchical keys
        let entries =
              [ (ObjectKey "foo/bar.txt",   "bar content")
              , (ObjectKey "foo/baz.txt",   "baz content")
              , (ObjectKey "foo/qux/nested", "nested content")
              , (ObjectKey "bar.txt",       "root content")
              , (ObjectKey "alpha.txt",     "alpha content")
              ]
        forM_ entries $ \(key, content) -> do
          (sha, sz, et) <- putObject tmp (yield content)
          _ <- putObjectMeta store bucket key sha sz Nothing [] et
          pure ()

        -- List with prefix "foo/" and delimiter "/"
        (objects, prefixes) <- listObjects store bucket (Just "foo/") (Just "/") Nothing 100

        -- Should get foo/bar.txt and foo/baz.txt as objects (not foo/qux/nested)
        -- Should get foo/qux/ as a common prefix
        let objectKeys = map oiKey objects
        objectKeys `shouldBe` [ObjectKey "foo/bar.txt", ObjectKey "foo/baz.txt"]
        prefixes `shouldBe` ["foo/qux/"]

        -- List without prefix to see everything
        (allObjects, allPrefixes) <- listObjects store bucket Nothing Nothing Nothing 100
        let allKeys = map oiKey allObjects
        -- With no delimiter, all keys should appear as objects
        length allKeys `shouldBe` length entries
        allPrefixes `shouldBe` []
