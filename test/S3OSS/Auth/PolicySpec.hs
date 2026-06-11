{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.PolicySpec (spec) where

import Test.Hspec
import S3OSS.Auth.Policy
import S3OSS.Types

spec :: Spec
spec = do
  describe "Policy engine" $ do
    it "allows when Allow policy matches" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` True

    it "denies when no policy matches (default deny)" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies S3PutObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "deny overrides allow" $ do
      let policies =
            [ Policy Allow [S3AllActions] [ResourceARN "*"]
            , Policy Deny  [S3DeleteObject] [ResourceARN "arn:aws:s3:::protected/*"]
            ]
      evaluate policies S3DeleteObject (ResourceARN "arn:aws:s3:::protected/important.db") `shouldBe` False
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::protected/important.db") `shouldBe` True

    it "wildcard action (S3AllActions) matches any specific action" $ do
      let policies = [Policy Allow [S3AllActions] [ResourceARN "*"]]
      evaluate policies S3PutObject (ResourceARN "arn:aws:s3:::any-bucket/any-key") `shouldBe` True

    it "wildcard resource (*) matches anything" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::some-bucket/some-key") `shouldBe` True

    it "prefix wildcard matches sub-resources" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/sub/dir/file.txt") `shouldBe` True

    it "exact ARN matches only that exact resource" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/exact-file.txt"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/exact-file.txt") `shouldBe` True
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/other-file.txt") `shouldBe` False
