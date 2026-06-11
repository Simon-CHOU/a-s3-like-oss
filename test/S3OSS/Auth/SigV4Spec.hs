{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.SigV4Spec (spec) where

import Test.Hspec
import S3OSS.Auth.SigV4
import S3OSS.Types
import qualified Data.ByteString as B

spec :: Spec
spec = do
  describe "SigV4 Signing Key Derivation" $ do
    it "produces a non-empty signing key" $ do
      let secret = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let date = "20150830"
      let region = "us-east-1"
      let service = "iam"
      let signingKey = deriveSigningKey secret date region service
      B.length signingKey `shouldSatisfy` (> 0)
      -- Signing key should be exactly 32 bytes (SHA-256)
      B.length signingKey `shouldBe` 32
