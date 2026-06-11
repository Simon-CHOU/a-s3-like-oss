{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the Auth + Handler layers.
--
-- Each test creates a WAI 'Application' backed by a single user with specific
-- policies, exercises an HTTP endpoint, and asserts the response status code.
-- Because the server is configured in development mode, 'resolveUser' returns
-- the sole configured user without requiring a valid SigV4 signature.
module S3OSS.AuthIntegrationSpec (spec) where

import Test.Hspec
import S3OSS.Server (mkApp)
import S3OSS.Config (ResolvedConfig(..), ServerConfig(..), StorageConfig(..))
import S3OSS.Store (initStore)
import S3OSS.Types
import Network.Wai (Application, requestMethod, requestHeaders)
import Network.Wai.Test
  ( runSession, SRequest(..), srequest, defaultRequest
  , setPath, simpleStatus
  )
import Network.HTTP.Types (status200, status403)
import System.IO.Temp (withSystemTempDirectory)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

-- | Build a 'User' from a policy list with fixed dummy credentials.
mkUser :: [Policy] -> User
mkUser policies =
  User "test-user" (AccessKey "AKID") (SecretKey "secret") policies

-- | Create a fresh store and WAI 'Application' inside the given temp
-- directory, configuring a single user with the supplied policies in
-- development mode (no SigV4 signing required).
withUserApp :: [Policy] -> FilePath -> IO Application
withUserApp policies tmpDir = do
  store <- initStore tmpDir
  let config = ResolvedConfig
        { rcServer  = ServerConfig "127.0.0.1" 9443 Nothing Nothing True
        , rcStorage = StorageConfig tmpDir
        , rcUsers   = [mkUser policies]
        }
  mkApp config store

-- --------------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------------

spec :: Spec
spec = describe "Auth Integration" $ do

  ------------------------------------------------------------------
  -- 1.  Allow S3PutObject → PUT object succeeds
  ------------------------------------------------------------------
  it "allows PUT object when user has S3PutObject policy" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket]      [ResourceARN "arn:aws:s3:::test-bucket"]
        , Policy Allow [S3PutObject]         [ResourceARN "arn:aws:s3:::test-bucket/*"]
        ] tmp

      -- Create the bucket first.
      let createReq = setPath (defaultRequest { requestMethod = "PUT" })
                               "/test-bucket"
      s1 <- simpleStatus <$> runSession (srequest (SRequest createReq "")) app
      s1 `shouldBe` status200

      -- Now PUT an object — should be allowed.
      let putReq = setPath (defaultRequest { requestMethod = "PUT" })
                           "/test-bucket/my-key"
      s2 <- simpleStatus <$> runSession (srequest (SRequest putReq "hello")) app
      s2 `shouldBe` status200

  ------------------------------------------------------------------
  -- 2.  No S3PutObject → PUT returns 403
  ------------------------------------------------------------------
  it "rejects PUT object when user lacks S3PutObject" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket] [ResourceARN "arn:aws:s3:::test-bucket"]
        ] tmp

      -- Create the bucket first.
      let createReq = setPath (defaultRequest { requestMethod = "PUT" })
                               "/test-bucket"
      _ <- runSession (srequest (SRequest createReq "")) app

      -- PUT object — user has no S3PutObject → AccessDenied.
      let putReq = setPath (defaultRequest { requestMethod = "PUT" })
                           "/test-bucket/my-key"
      s <- simpleStatus <$> runSession (srequest (SRequest putReq "hello")) app
      s `shouldBe` status403

  ------------------------------------------------------------------
  -- 3.  Explicit Deny overrides Allow → PUT returns 403
  ------------------------------------------------------------------
  it "rejects PUT object when explicit Deny overrides Allow" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket]      [ResourceARN "arn:aws:s3:::test-bucket"]
        , Policy Allow [S3PutObject]         [ResourceARN "*"]
        , Policy Deny  [S3PutObject]         [ResourceARN "arn:aws:s3:::test-bucket/*"]
        ] tmp

      -- Create the bucket first.
      let createReq = setPath (defaultRequest { requestMethod = "PUT" })
                               "/test-bucket"
      _ <- runSession (srequest (SRequest createReq "")) app

      -- PUT object — Deny overrides Allow → AccessDenied.
      let putReq = setPath (defaultRequest { requestMethod = "PUT" })
                           "/test-bucket/my-key"
      s <- simpleStatus <$> runSession (srequest (SRequest putReq "hello")) app
      s `shouldBe` status403

  ------------------------------------------------------------------
  -- 4.  CopyObject with GetObject on src + PutObject on dst → succeeds
  ------------------------------------------------------------------
  it "allows CopyObject when user has GetObject on src and PutObject on dst" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket]      [ResourceARN "*"]
        , Policy Allow [S3PutObject, S3GetObject] [ResourceARN "*"]
        ] tmp

      -- Create source bucket.
      let createSrc = setPath (defaultRequest { requestMethod = "PUT" })
                               "/src-bucket"
      _ <- runSession (srequest (SRequest createSrc "")) app

      -- Create destination bucket.
      let createDst = setPath (defaultRequest { requestMethod = "PUT" })
                               "/dst-bucket"
      _ <- runSession (srequest (SRequest createDst "")) app

      -- PUT a source object.
      let putSrc = setPath (defaultRequest { requestMethod = "PUT" })
                            "/src-bucket/source-key"
      _ <- runSession (srequest (SRequest putSrc "source-data")) app

      -- CopyObject: PUT /dst-bucket/dst-key with x-amz-copy-source header.
      let copyReq = (setPath (defaultRequest { requestMethod = "PUT" })
                              "/dst-bucket/dst-key")
            { requestHeaders = [("x-amz-copy-source", "src-bucket/source-key")] }
      s <- simpleStatus <$> runSession (srequest (SRequest copyReq "")) app
      s `shouldBe` status200

  ------------------------------------------------------------------
  -- 5.  CopyObject with only GetObject on src → 403
  ------------------------------------------------------------------
  it "rejects CopyObject when user lacks PutObject on dst" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket]                   [ResourceARN "*"]
        , Policy Allow [S3PutObject, S3GetObject]         [ResourceARN "arn:aws:s3:::src-bucket/*"]
        ] tmp

      -- Create source bucket.
      let createSrc = setPath (defaultRequest { requestMethod = "PUT" })
                               "/src-bucket"
      _ <- runSession (srequest (SRequest createSrc "")) app

      -- Create destination bucket (allowed via S3CreateBucket on "*").
      let createDst = setPath (defaultRequest { requestMethod = "PUT" })
                               "/dst-bucket"
      _ <- runSession (srequest (SRequest createDst "")) app

      -- PUT source object (allowed via src-bucket/* policy).
      let putSrc = setPath (defaultRequest { requestMethod = "PUT" })
                            "/src-bucket/source-key"
      _ <- runSession (srequest (SRequest putSrc "source-data")) app

      -- CopyObject to dst — user has no PutObject on dst-bucket.
      let copyReq = (setPath (defaultRequest { requestMethod = "PUT" })
                              "/dst-bucket/dst-key")
            { requestHeaders = [("x-amz-copy-source", "src-bucket/source-key")] }
      s <- simpleStatus <$> runSession (srequest (SRequest copyReq "")) app
      s `shouldBe` status403

  ------------------------------------------------------------------
  -- 6.  ListBuckets with wildcard resource
  ------------------------------------------------------------------
  it "allows ListBuckets with wildcard resource" $ do
    withSystemTempDirectory "s3oss-auth-int" $ \tmp -> do
      app <- withUserApp
        [ Policy Allow [S3CreateBucket, S3ListBuckets] [ResourceARN "*"]
        ] tmp

      -- Create a bucket so there is something to list.
      let createReq = setPath (defaultRequest { requestMethod = "PUT" })
                               "/my-bucket"
      _ <- runSession (srequest (SRequest createReq "")) app

      -- ListBuckets — GET /
      let listReq = setPath defaultRequest "/"
      s <- simpleStatus <$> runSession (srequest (SRequest listReq "")) app
      s `shouldBe` status200
