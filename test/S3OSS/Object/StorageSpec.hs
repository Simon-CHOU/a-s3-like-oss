{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Object.StorageSpec (spec) where

import Test.Hspec
import S3OSS.Object.Storage
import S3OSS.Types
import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import System.IO.Temp (withSystemTempDirectory)
import Data.Conduit ((.|), yield, runConduitRes)

spec :: Spec
spec = do
  describe "Object Storage" $ do
    it "writes and reads an object correctly" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = "hello, world!" :: B.ByteString
        let source = yield content
        (sha256, size, etag) <- putObject tmp source
        size `shouldBe` fromIntegral (B.length content)
        -- Read it back
        result <- runConduitRes $ getObject tmp sha256 .| CL.consume
        B.concat result `shouldBe` content

    it "writes empty object correctly" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = "" :: B.ByteString
        let source = yield content
        (sha256, size, _) <- putObject tmp source
        size `shouldBe` 0
        result <- runConduitRes $ getObject tmp sha256 .| CL.consume
        result `shouldBe` []

    it "deduplicates identical content (same hash)" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = "dedup-test-content" :: B.ByteString
        (hash1, _, _) <- putObject tmp (yield content)
        (hash2, _, _) <- putObject tmp (yield content)
        hash1 `shouldBe` hash2
