{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for S3OSS.Bucket.Handler.
module S3OSS.Bucket.HandlerSpec (spec) where

import Test.Hspec
import S3OSS.Bucket.Handler
import S3OSS.Store
import S3OSS.Types
import S3OSS.Auth.Policy
import S3OSS.Object.Storage (putObject)
import Network.Wai (Response, responseStatus)
import Network.HTTP.Types (status200, status204, status403, status404, status409)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as B
import Data.Conduit (yield)

-- | Helper to build a User with the given policies.
mkUser :: [Policy] -> User
mkUser policies = User "test-user" (AccessKey "AKID") (SecretKey "secret") policies

spec :: Spec
spec = do
  describe "handleCreateBucket" $ do
    it "allows creation when policy grants S3CreateBucket on the bucket ARN" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "my-bucket"
        let user   = mkUser [Policy Allow [S3CreateBucket] [ResourceARN "arn:aws:s3:::my-bucket"]]
        resp <- handleCreateBucket store user bucket
        responseStatus resp `shouldBe` status200

    it "denies creation when policy explicitly denies S3CreateBucket" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "my-bucket"
        let user   = mkUser [Policy Deny [S3CreateBucket] [ResourceARN "arn:aws:s3:::my-bucket"]]
        resp <- handleCreateBucket store user bucket
        responseStatus resp `shouldBe` status403

    it "default-denies creation when no policy matches S3CreateBucket" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "my-bucket"
        -- User has Allow for S3GetObject, but no policy for S3CreateBucket.
        let user   = mkUser [Policy Allow [S3GetObject] [ResourceARN "*"]]
        resp <- handleCreateBucket store user bucket
        responseStatus resp `shouldBe` status403

  describe "handleDeleteBucket" $ do
    it "rejects deleting a non-empty bucket with HTTP 409" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "my-bucket"
        let user   = mkUser [Policy Allow [S3CreateBucket, S3DeleteBucket, S3PutObject] [ResourceARN "*"]]
        -- Create the bucket.
        _ <- handleCreateBucket store user bucket
        -- Place an object in the bucket.
        let content = "some-data" :: B.ByteString
        (hash, size, etag) <- putObject tmp (yield content)
        _ <- putObjectMeta store bucket (ObjectKey "my-key") hash size Nothing [] etag
        -- Attempt to delete the non-empty bucket.
        resp <- handleDeleteBucket store user bucket
        responseStatus resp `shouldBe` status409

    it "deletes an empty bucket successfully" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "my-bucket"
        let user   = mkUser [Policy Allow [S3CreateBucket, S3DeleteBucket] [ResourceARN "*"]]
        _ <- handleCreateBucket store user bucket
        resp <- handleDeleteBucket store user bucket
        responseStatus resp `shouldBe` status204

  describe "handleListBuckets" $ do
    it "returns HTTP 200 with multiple buckets" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let user = mkUser [Policy Allow [S3CreateBucket, S3ListBuckets] [ResourceARN "*"]]
        let bucketA = BucketName "alpha"
        let bucketB = BucketName "beta"
        let bucketC = BucketName "gamma"
        _ <- handleCreateBucket store user bucketA
        _ <- handleCreateBucket store user bucketB
        _ <- handleCreateBucket store user bucketC
        resp <- handleListBuckets store user
        responseStatus resp `shouldBe` status200

  describe "handleHeadBucket" $ do
    it "returns HTTP 404 for a non-existent bucket" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "non-existent"
        let user   = mkUser [Policy Allow [S3HeadBucket] [ResourceARN "*"]]
        resp <- handleHeadBucket store user bucket
        responseStatus resp `shouldBe` status404

    it "returns HTTP 200 for an existing bucket" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "existing-bucket"
        let user   = mkUser [Policy Allow [S3CreateBucket, S3HeadBucket] [ResourceARN "*"]]
        _ <- handleCreateBucket store user bucket
        resp <- handleHeadBucket store user bucket
        responseStatus resp `shouldBe` status200

    it "denies when policy does not allow S3HeadBucket" $ do
      withSystemTempDirectory "s3oss-handler-test" $ \tmp -> do
        store <- initStore tmp
        let bucket = BucketName "some-bucket"
        let user   = mkUser [Policy Allow [S3CreateBucket] [ResourceARN "*"]]
        _ <- handleCreateBucket store user bucket
        resp <- handleHeadBucket store user bucket
        responseStatus resp `shouldBe` status403
