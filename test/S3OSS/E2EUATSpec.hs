{-# LANGUAGE OverloadedStrings #-}

-- | E2E UAT tests for the S3-like OSS server.
--
-- Each test starts a real Warp server on a random port with a temp data
-- directory in development mode, then issues real HTTP requests via
-- @http-conduit@ ('Network.HTTP.Simple').  This exercises the full HTTP
-- stack including WAI routing, SigV4 resolution (dev-mode shortcut),
-- policy evaluation, and storage I/O.
module S3OSS.E2EUATSpec (spec) where

import Test.Hspec
import S3OSS.Server (mkApp)
import S3OSS.Config
  ( defaultConfig, ResolvedConfig(..), ServerConfig(..), StorageConfig(..) )
import S3OSS.Store (initStore)
import S3OSS.Types
import Network.Wai.Handler.Warp
  ( runSettings, defaultSettings, setPort, setHost )
import Network.HTTP.Simple
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket, try, SomeException)
import Data.Maybe (fromMaybe)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML as X
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Socket
  ( socket, bind, close, SockAddr(..), Family(..), SocketType(..)
  , defaultProtocol, socketPort, setSocketOption, SocketOption(..)
  )
import Network.HTTP.Types (hETag)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Find a free TCP port by binding to port 0, then releasing it.
-- There is a tiny race window between release and use, which is
-- acceptable for test purposes.
findFreePort :: IO Int
findFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 0)
  port <- socketPort sock
  close sock
  pure $ fromIntegral port

-- | Poll the server endpoint until it responds (up to ~5 seconds).
waitForServer :: Int -> IO ()
waitForServer port = go (50 :: Int)
  where
    go 0 = pure ()
    go n = do
      ready <- tryPing port
      if ready then pure ()
      else do threadDelay 100_000; go (n - 1)

    tryPing :: Int -> IO Bool
    tryPing p = do
      result <- try (parseRequest (urlFor p "/") >>= httpLBS)
                :: IO (Either SomeException (Response BL.ByteString))
      pure $ case result of
        Right _ -> True
        Left  _ -> False

-- | Start an S3 server in a background thread, run the given action,
-- then shut the server down.
withTestServer :: ResolvedConfig -> FilePath -> (Int -> IO a) -> IO a
withTestServer config dataDir action = do
  port <- findFreePort
  let finalConfig = config
        { rcStorage = StorageConfig dataDir
        , rcServer  = (rcServer config) { scPort = port }
        }
  bracket
    (do
      store <- initStore dataDir
      app   <- mkApp finalConfig store
      let settings = setPort port $ setHost "127.0.0.1" defaultSettings
      tid <- forkIO $ runSettings settings app
      waitForServer port
      pure tid)
    killThread
    (\_ -> action port)

-- | Build a URL for the server running on @port@.
urlFor :: Int -> String -> String
urlFor port path = "http://127.0.0.1:" ++ show port ++ path

-- | Build the XML body for a 'CompleteMultipartUpload' request.
buildCompleteBody :: [(Int, T.Text)] -> BL.ByteString
buildCompleteBody parts =
  BL.fromStrict
    (  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    <> "<CompleteMultipartUpload>"
    <> mconcat (map partXml parts)
    <> "</CompleteMultipartUpload>"
    )
  where
    partXml (num, etag) =
         "<Part><PartNumber>"
      <> encodeUtf8 (T.pack (show num))
      <> "</PartNumber><ETag>"
      <> encodeUtf8 etag
      <> "</ETag></Part>"

-- | Extract @UploadId@ text from an 'InitiateMultipartUploadResult'
-- XML body using xml-conduit parsing (handles namespace attributes
-- that the S3 XML renderer emits for namespace-less child elements).
extractUploadId :: BL.ByteString -> T.Text
extractUploadId body =
  case X.parseLBS X.def body of
    Left err  -> error $ "extractUploadId: XML parse error: " ++ show err
    Right doc -> case childElem "UploadId" (X.documentRoot doc) of
      Just t  -> t
      Nothing -> error $ "extractUploadId: <UploadId> not found in: "
                      ++ T.unpack (decodeUtf8 (BL.toStrict (BL.take 300 body)))
  where
    childElem name el =
      case [textContent e | e <- childElements el
           , X.nameLocalName (X.elementName e) == name] of
        (t:_) -> Just t
        []    -> Nothing
    childElements el = [e | X.NodeElement e <- X.elementNodes el]
    textContent el = T.strip $ T.concat
                       [t | X.NodeContent t <- X.elementNodes el]

-- | Look up a response header by name, returning its value as 'Text'.
headerValue :: Eq a => [(a, B.ByteString)] -> a -> Maybe T.Text
headerValue hdrs name =
  fmap decodeUtf8 (lookup name hdrs)


--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "E2E UAT" $ do

  ------------------------------------------------------------------
  -- 1. Full basic S3 workflow
  ------------------------------------------------------------------
  it "full S3 workflow: CreateBucket -> PutObject -> GetObject \
     \(verify) -> HeadObject -> ListObjects -> DeleteObject -> \
     \DeleteBucket" $
    withSystemTempDirectory "s3oss-e2e" $ \tmpDir ->
      withTestServer defaultConfig tmpDir $ \port -> do

        -- (1) CreateBucket
        rCreate <- parseRequest (urlFor port "/e2e-uat-bucket")
             >>= \req -> httpLBS (setRequestMethod "PUT" req)
        getResponseStatusCode rCreate `shouldBe` 200

        -- (2) PutObject
        let content = "Hello, E2E World!" :: BL.ByteString
        rPut <- parseRequest (urlFor port "/e2e-uat-bucket/hello.txt")
             >>= \req -> httpLBS
                    (setRequestBodyLBS content
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rPut `shouldBe` 200

        -- (3) GetObject (verify content)
        rGet <- parseRequest (urlFor port "/e2e-uat-bucket/hello.txt")
             >>= httpLBS
        getResponseStatusCode rGet `shouldBe` 200
        getResponseBody rGet `shouldBe` content

        -- (4) HeadObject
        rHead <- parseRequest (urlFor port "/e2e-uat-bucket/hello.txt")
             >>= \req -> httpLBS (setRequestMethod "HEAD" req)
        getResponseStatusCode rHead `shouldBe` 200

        -- (5) ListObjects
        rList <- parseRequest (urlFor port "/e2e-uat-bucket") >>= httpLBS
        getResponseStatusCode rList `shouldBe` 200
        -- XML body must reference the stored key
        B.isInfixOf "hello.txt" (BL.toStrict (getResponseBody rList)) `shouldBe` True

        -- (6) DeleteObject
        rDel <- parseRequest (urlFor port "/e2e-uat-bucket/hello.txt")
             >>= \req -> httpLBS (setRequestMethod "DELETE" req)
        getResponseStatusCode rDel `shouldBe` 204

        -- (7) DeleteBucket
        rDelBucket <- parseRequest (urlFor port "/e2e-uat-bucket")
             >>= \req -> httpLBS (setRequestMethod "DELETE" req)
        getResponseStatusCode rDelBucket `shouldBe` 204

  ------------------------------------------------------------------
  -- 2. Multipart upload E2E
  ------------------------------------------------------------------
  it "multipart E2E: CreateMultipartUpload -> UploadPart (x3) -> \
     \CompleteMultipartUpload -> GetObject (verify assembled)" $
    withSystemTempDirectory "s3oss-e2e-mp" $ \tmpDir ->
      withTestServer defaultConfig tmpDir $ \port -> do

        -- Create bucket first
        rMpCreate <- parseRequest (urlFor port "/mp-e2e-bucket")
             >>= \req -> httpLBS (setRequestMethod "PUT" req)
        getResponseStatusCode rMpCreate `shouldBe` 200

        -- CreateMultipartUpload
        rMpInit <- parseRequest
               (urlFor port "/mp-e2e-bucket/large-file.bin?uploads")
             >>= \req -> httpLBS (setRequestMethod "POST" req)
        getResponseStatusCode rMpInit `shouldBe` 200
        let uploadId = extractUploadId (getResponseBody rMpInit)

        -- Part data
        let part1 = BL.replicate (64 * 1024) 0x41   -- 64 KB of 'A'
        let part2 = BL.replicate (32 * 1024) 0x42   -- 32 KB of 'B'
        let part3 = "final-chunk" :: BL.ByteString

        -- Upload part 1
        rPart1 <- parseRequest
               (urlFor port
                  ("/mp-e2e-bucket/large-file.bin?uploadId="
                    ++ T.unpack uploadId ++ "&partNumber=1"))
             >>= \req -> httpLBS
                    (setRequestBodyLBS part1
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rPart1 `shouldBe` 200
        let etag1 = fromMaybe
              (error "E2EUATSpec: missing ETag header in part 1") $
              headerValue (getResponseHeaders rPart1) hETag

        -- Upload part 2
        rPart2 <- parseRequest
               (urlFor port
                  ("/mp-e2e-bucket/large-file.bin?uploadId="
                    ++ T.unpack uploadId ++ "&partNumber=2"))
             >>= \req -> httpLBS
                    (setRequestBodyLBS part2
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rPart2 `shouldBe` 200
        let etag2 = fromMaybe
              (error "E2EUATSpec: missing ETag header in part 2") $
              headerValue (getResponseHeaders rPart2) hETag

        -- Upload part 3
        rPart3 <- parseRequest
               (urlFor port
                  ("/mp-e2e-bucket/large-file.bin?uploadId="
                    ++ T.unpack uploadId ++ "&partNumber=3"))
             >>= \req -> httpLBS
                    (setRequestBodyLBS part3
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rPart3 `shouldBe` 200
        let etag3 = fromMaybe
              (error "E2EUATSpec: missing ETag header in part 3") $
              headerValue (getResponseHeaders rPart3) hETag

        -- CompleteMultipartUpload
        let completeBody = buildCompleteBody
              [(1, etag1), (2, etag2), (3, etag3)]
        rComplete <- parseRequest
               (urlFor port
                  ("/mp-e2e-bucket/large-file.bin?uploadId="
                    ++ T.unpack uploadId))
             >>= \req -> httpLBS
                    (setRequestBodyLBS completeBody
                    (setRequestMethod "POST" req))
        getResponseStatusCode rComplete `shouldBe` 200

        -- GetObject -- verify assembled content matches
        rMpGet <- parseRequest
               (urlFor port "/mp-e2e-bucket/large-file.bin")
             >>= httpLBS
        getResponseStatusCode rMpGet `shouldBe` 200
        getResponseBody rMpGet `shouldBe` part1 <> part2 <> part3

  ------------------------------------------------------------------
  -- 3. Abort multipart upload
  ------------------------------------------------------------------
  it "abort multipart: CreateMultipartUpload -> UploadPart -> \
     \AbortMultipartUpload" $
    withSystemTempDirectory "s3oss-e2e-abort" $ \tmpDir ->
      withTestServer defaultConfig tmpDir $ \port -> do

        -- Create bucket
        rAbortCreate <- parseRequest (urlFor port "/abort-e2e-bucket")
             >>= \req -> httpLBS (setRequestMethod "PUT" req)
        getResponseStatusCode rAbortCreate `shouldBe` 200

        -- CreateMultipartUpload
        rAbortInit <- parseRequest
               (urlFor port "/abort-e2e-bucket/abort-me.bin?uploads")
             >>= \req -> httpLBS (setRequestMethod "POST" req)
        getResponseStatusCode rAbortInit `shouldBe` 200
        let uploadId = extractUploadId (getResponseBody rAbortInit)

        -- Upload one part
        rAbortPart <- parseRequest
               (urlFor port
                  ("/abort-e2e-bucket/abort-me.bin?uploadId="
                    ++ T.unpack uploadId ++ "&partNumber=1"))
             >>= \req -> httpLBS
                    (setRequestBodyLBS "part-data"
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rAbortPart `shouldBe` 200

        -- AbortMultipartUpload
        rAbortDel <- parseRequest
               (urlFor port
                  ("/abort-e2e-bucket/abort-me.bin?uploadId="
                    ++ T.unpack uploadId))
             >>= \req -> httpLBS (setRequestMethod "DELETE" req)
        getResponseStatusCode rAbortDel `shouldBe` 204

  ------------------------------------------------------------------
  -- 4. Auth enforcement
  ------------------------------------------------------------------
  it "returns 403 for operations not covered by user policy" $
    withSystemTempDirectory "s3oss-e2e-auth" $ \tmpDir -> do
      -- Create a config with a user whose policies cover only
      -- CreateBucket and PutObject on a specific bucket.
      let limitedUser = User
            { userName      = "limited-user"
            , userAccessKey = AccessKey "AKID0000000000000000"
            , userSecretKey = SecretKey "dev-secret-key-change-me"
            , userPolicies  =
                [ Policy Allow [S3CreateBucket]
                    [ResourceARN "arn:aws:s3:::auth-test-bucket"]
                , Policy Allow [S3PutObject]
                    [ResourceARN "arn:aws:s3:::auth-test-bucket/*"]
                ]
            }
      let limitedConfig = defaultConfig { rcUsers = [limitedUser] }

      withTestServer limitedConfig tmpDir $ \port -> do

        -- CreateBucket -- allowed (in policy)
        rAuthCreate <- parseRequest (urlFor port "/auth-test-bucket")
             >>= \req -> httpLBS (setRequestMethod "PUT" req)
        getResponseStatusCode rAuthCreate `shouldBe` 200

        -- PutObject -- allowed (in policy)
        rAuthPut <- parseRequest
               (urlFor port "/auth-test-bucket/allowed-key.txt")
             >>= \req -> httpLBS
                    (setRequestBodyLBS "hello"
                    (setRequestMethod "PUT" req))
        getResponseStatusCode rAuthPut `shouldBe` 200

        -- ListObjects -- denied (not in policy) -> 403
        rAuthList <- parseRequest (urlFor port "/auth-test-bucket")
             >>= httpLBS
        getResponseStatusCode rAuthList `shouldBe` 403

        -- DeleteObject -- denied -> 403
        rAuthDel <- parseRequest
               (urlFor port "/auth-test-bucket/allowed-key.txt")
             >>= \req -> httpLBS (setRequestMethod "DELETE" req)
        getResponseStatusCode rAuthDel `shouldBe` 403

        -- DeleteBucket -- denied -> 403
        rAuthDelBucket <- parseRequest (urlFor port "/auth-test-bucket")
             >>= \req -> httpLBS (setRequestMethod "DELETE" req)
        getResponseStatusCode rAuthDelBucket `shouldBe` 403

        -- HeadObject -- denied -> 403
        rAuthHead <- parseRequest
               (urlFor port "/auth-test-bucket/allowed-key.txt")
             >>= \req -> httpLBS (setRequestMethod "HEAD" req)
        getResponseStatusCode rAuthHead `shouldBe` 403

        -- GetObject -- denied -> 403
        rAuthGet <- parseRequest
               (urlFor port "/auth-test-bucket/allowed-key.txt")
             >>= httpLBS
        getResponseStatusCode rAuthGet `shouldBe` 403
