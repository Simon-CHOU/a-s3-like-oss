{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.PolicySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import S3OSS.Auth.Policy
import S3OSS.Types

--------------------------------------------------------------------------------
-- QuickCheck Generators
--------------------------------------------------------------------------------

-- | Generate any Action, including S3AllActions.
genAction :: Gen Action
genAction = elements
  [ S3GetObject, S3PutObject, S3DeleteObject, S3HeadObject
  , S3CopyObject, S3ListObjects, S3CreateBucket, S3DeleteBucket
  , S3ListBuckets, S3HeadBucket, S3CreateMultipartUpload
  , S3UploadPart, S3CompleteMultipartUpload, S3AbortMultipartUpload
  , S3AllActions
  ]

-- | Generate a specific Action (never S3AllActions).
genSpecificAction :: Gen Action
genSpecificAction = elements
  [ S3GetObject, S3PutObject, S3DeleteObject, S3HeadObject
  , S3CopyObject, S3ListObjects, S3CreateBucket, S3DeleteBucket
  , S3ListBuckets, S3HeadBucket, S3CreateMultipartUpload
  , S3UploadPart, S3CompleteMultipartUpload, S3AbortMultipartUpload
  ]

-- | Generate a resource ARN pattern (including wildcards).
genResourcePattern :: Gen ResourceARN
genResourcePattern = elements
  [ ResourceARN "*"
  , ResourceARN "arn:aws:s3:::my-bucket/*"
  , ResourceARN "arn:aws:s3:::my-bucket/foo.txt"
  , ResourceARN "arn:aws:s3:::my-bucket/report.txt"
  , ResourceARN "*.txt"
  , ResourceARN "*.pdf"
  ]

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

  describe "QuickCheck properties" $ do

    it "empty policy list always denies" $ property $
      forAll genAction $ \action ->
      forAll genResourcePattern $ \resource ->
        evaluate [] action resource == False

    it "universal Allow (S3AllActions + *) matches any request" $ property $
      forAll genAction $ \action ->
      forAll genResourcePattern $ \resource ->
        evaluate [Policy Allow [S3AllActions] [ResourceARN "*"]] action resource == True

    it "universal Deny (S3AllActions + *) denies any request" $ property $
      forAll genAction $ \action ->
      forAll genResourcePattern $ \resource ->
        evaluate [Policy Deny [S3AllActions] [ResourceARN "*"]] action resource == False

    it "single exact-match Allow evaluates to True" $ property $
      forAll genSpecificAction $ \action ->
        let resource = ResourceARN "arn:aws:s3:::my-bucket/foo.txt"
        in evaluate [Policy Allow [action] [resource]] action resource == True

    it "Deny overrides Allow regardless of order" $ property $
      forAll genSpecificAction $ \action ->
      forAll genResourcePattern $ \resource ->
        let allowFirst = [Policy Allow [action] [resource], Policy Deny [action] [resource]]
            denyFirst  = [Policy Deny  [action] [resource], Policy Allow [action] [resource]]
        in evaluate allowFirst action resource == False
        && evaluate denyFirst action resource == False

    it "empty actions list in Allow is inert" $ property $
      forAll genSpecificAction $ \action ->
      forAll genResourcePattern $ \resource ->
        let base = [Policy Allow [action] [resource]]
            withEmpty = base ++ [Policy Allow [] [ResourceARN "*"]]
        in evaluate base action resource == evaluate withEmpty action resource

    it "empty actions list in Deny is inert" $ property $
      forAll genSpecificAction $ \action ->
      forAll genResourcePattern $ \resource ->
        let base = [Policy Allow [action] [resource]]
            withEmptyDeny = base ++ [Policy Deny [] [ResourceARN "*"]]
        in evaluate base action resource == evaluate withEmptyDeny action resource

    it "empty resources list in Allow is inert" $ property $
      forAll genSpecificAction $ \action ->
        let resource = ResourceARN "arn:aws:s3:::my-bucket/foo.txt"
            base = [Policy Allow [action] [resource]]
            withEmptyRes = base ++ [Policy Allow [action] []]
        in evaluate base action resource == evaluate withEmptyRes action resource

    it "empty resources list in Deny is inert" $ property $
      forAll genSpecificAction $ \action ->
        let resource = ResourceARN "arn:aws:s3:::my-bucket/foo.txt"
            base = [Policy Allow [action] [resource]]
            withEmptyDeny = base ++ [Policy Deny [action] []]
        in evaluate base action resource == evaluate withEmptyDeny action resource
