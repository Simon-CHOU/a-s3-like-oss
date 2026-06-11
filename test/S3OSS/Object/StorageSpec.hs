{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Object.StorageSpec (spec) where

import Test.Hspec
import S3OSS.Object.Storage
import S3OSS.Types
import Data.Int (Int64)
import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import System.IO.Temp (withSystemTempDirectory)
import Data.Conduit ((.|), yield, runConduitRes)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import Data.List (isInfixOf)
import qualified Data.Text as T

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
        -- Also verify SHA-256 is returned correctly (non-empty, hex format)
        T.length (unSha256Hex sha256) `shouldBe` 64
        -- ETag should be a quoted string
        T.head (unETag etag) `shouldBe` '\"'
        T.last (unETag etag) `shouldBe` '\"'

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

    it "handles concurrent writes correctly" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content1 = "concurrent-content-1" :: B.ByteString
        let content2 = "concurrent-content-2" :: B.ByteString
        var1 <- newEmptyMVar :: IO (MVar (Either SomeException (Sha256Hex, Int64, ETag)))
        var2 <- newEmptyMVar :: IO (MVar (Either SomeException (Sha256Hex, Int64, ETag)))
        forkIO $ do
          result <- try (putObject tmp (yield content1))
          putMVar var1 result
        forkIO $ do
          result <- try (putObject tmp (yield content2))
          putMVar var2 result
        r1 <- takeMVar var1
        r2 <- takeMVar var2
        (h1, s1, _) <- either throwIO pure r1
        (h2, s2, _) <- either throwIO pure r2
        h1 `shouldNotBe` h2
        s1 `shouldBe` fromIntegral (B.length content1)
        s2 `shouldBe` fromIntegral (B.length content2)
        result1 <- runConduitRes $ getObject tmp h1 .| CL.consume
        result2 <- runConduitRes $ getObject tmp h2 .| CL.consume
        B.concat result1 `shouldBe` content1
        B.concat result2 `shouldBe` content2

    it "streams large objects correctly" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = B.replicate (1024 * 1024) 0x41
        let source = yield content
        (sha256, size, _) <- putObject tmp source
        size `shouldBe` fromIntegral (B.length content)
        result <- runConduitRes $ getObject tmp sha256 .| CL.consume
        B.concat result `shouldBe` content

    it "cleans up temp files when putObject fails" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let failingSource = do
              yield ("some-data" :: B.ByteString)
              liftIO $ ioError (userError "simulated failure")
        putObject tmp failingSource `shouldThrow` anyException
        let objectsDir = tmp <> "/objects"
        dirExists <- doesDirectoryExist objectsDir
        when dirExists $ do
          files <- listDirectory objectsDir
          let tempFiles = filter (".tmp-upload" `isInfixOf`) files
          tempFiles `shouldBe` []

    it "atomically renames temp file to final path" $
      withSystemTempDirectory "s3oss-test" $ \tmp -> do
        let content = "atomic-rename-test" :: B.ByteString
        (sha256, _, _) <- putObject tmp (yield content)
        let sha256Text = unSha256Hex sha256
        let prefix = T.take 2 sha256Text
        let shardDir = tmp <> "/objects/" <> T.unpack prefix
        let finalPath = shardDir <> "/" <> T.unpack sha256Text
        fileExists <- doesFileExist finalPath
        fileExists `shouldBe` True
        let objectsDir = tmp <> "/objects"
        files <- listDirectory objectsDir
        let nonShardFiles = filter (\f -> f /= T.unpack prefix) files
        nonShardFiles `shouldBe` []
        result <- runConduitRes $ getObject tmp sha256 .| CL.consume
        B.concat result `shouldBe` content
