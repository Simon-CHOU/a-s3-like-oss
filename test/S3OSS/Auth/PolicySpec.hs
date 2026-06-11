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

  describe "Edge cases" $ do
    it "empty actions list defaults to deny" $ do
      let policies = [Policy Allow [] [ResourceARN "*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "empty resources list defaults to deny" $ do
      let policies = [Policy Allow [S3GetObject] []]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "empty actions in deny has no blocking effect" $ do
      let policies = [Policy Deny [] [ResourceARN "*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "empty resources in deny has no blocking effect" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "*"], Policy Deny [S3GetObject] []]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` True

  describe "Wildcard boundaries" $ do
    it "leading wildcard matches suffix" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "*.txt"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` True
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.md") `shouldBe` False

    it "multiple wildcards in resource pattern do not match" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "a*b*c"]]
      evaluate policies S3GetObject (ResourceARN "aXbYc") `shouldBe` False

    it "middle wildcard matches prefix and suffix" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*.txt"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/report.txt") `shouldBe` True
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/report.md") `shouldBe` False
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::other-bucket/report.txt") `shouldBe` False

    it "universal wildcard in resource list matches everything" $ do
      let policies = [Policy Allow [S3GetObject] [ResourceARN "*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::any/thing") `shouldBe` True
      evaluate policies S3PutObject (ResourceARN "arn:aws:s3:::any/thing") `shouldBe` False

  describe "Deny + Allow ordering" $ do
    it "deny overrides allow regardless of order (deny first)" $ do
      let policies =
            [ Policy Deny  [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]
            , Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]
            ]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "deny overrides allow regardless of order (allow first)" $ do
      let policies =
            [ Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]
            , Policy Deny  [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]
            ]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` False

    it "multiple denies with intermixed allows still results in deny" $ do
      let policies =
            [ Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::bucket/*"]
            , Policy Deny  [S3GetObject] [ResourceARN "arn:aws:s3:::bucket/secret/*"]
            , Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::bucket/secret/not-allowed/*"]
            ]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::bucket/secret/foo.txt") `shouldBe` False

  describe "Large policy set" $ do
    it "only matching allow policy grants access among many irrelevant policies" $ do
      let genPolicies eff action (n :: Int) = [Policy eff [action] [ResourceARN "arn:aws:s3:::other-bucket/*"] | _ <- [1..n]]
      let policies = genPolicies Allow S3PutObject 50
            ++ [Policy Allow [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/foo.txt") `shouldBe` True

    it "one deny among many allows still denies" $ do
      let genPolicies eff action (n :: Int) = [Policy eff [action] [ResourceARN "arn:aws:s3:::my-bucket/*"] | _ <- [1..n]]
      let policies = genPolicies Allow S3GetObject 100
            ++ [Policy Deny [S3GetObject] [ResourceARN "arn:aws:s3:::my-bucket/secret/*"]]
      evaluate policies S3GetObject (ResourceARN "arn:aws:s3:::my-bucket/secret/foo.txt") `shouldBe` False
