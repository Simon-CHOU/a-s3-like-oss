{-# LANGUAGE OverloadedStrings #-}

-- | WAI Application and server startup.
module S3OSS.Server where

import RIO
import S3OSS.Types
import S3OSS.Config
import S3OSS.Store
import S3OSS.Auth.SigV4
import S3OSS.Bucket.Handler hiding (errorResponse)
import S3OSS.Object.Handler hiding (errorResponse)
import S3OSS.Object.Storage (putObject)
import S3OSS.List.Handler hiding (errorResponse)
import S3OSS.Multipart.Handler hiding (errorResponse)
import S3OSS.Multipart.Manager
import S3OSS.XML (renderLBS, renderError)
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai hiding (lazyRequestBody)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, (.|), runConduit, yield)
import qualified Data.Conduit.List as CL
import Control.Concurrent (forkIO)
import qualified Data.Text.Encoding.Error as TEE

-- | Build the WAI Application.
mkApp :: ResolvedConfig -> Store -> IO Application
mkApp config store = do
  _ <- forkIO $ uploadGC store (stDataDir $ rcStorage config)
  pure $ app config store

-- | Resolve the authenticated user for a request.
resolveUser :: ResolvedConfig -> Request -> IO (Either Response User)
resolveUser config req =
  if scDevelopmentMode (rcServer config)
    then case rcUsers config of
           (u:_) -> pure $ Right u
           []    -> pure $ Left $ errorResponse status500 "InternalError" "No users configured"
    else do
      result <- verifySigV4 False (rcUsers config) req
      pure $ case result of
        Left err -> Left $ errorResponse status403 "SignatureDoesNotMatch" err
        Right user -> Right user

-- | Main application handler: route requests based on HTTP method and path.
app :: ResolvedConfig -> Store -> Application
app config store req respond = do
  let method = requestMethod req
      path'  = pathInfo req
      query  = queryString req
      dataDir = stDataDir $ rcStorage config

  mUser <- resolveUser config req

  result <- case (method, path', query) of
    -- GET / → ListBuckets
    ("GET", [], _) -> case mUser of
      Left err -> pure err
      Right user -> handleListBuckets store user

    -- PUT /{bucket} → CreateBucket
    ("PUT", [bucketName], _) -> case mUser of
      Left err -> pure err
      Right user -> handleCreateBucket store user (BucketName bucketName)

    -- DELETE /{bucket} → DeleteBucket
    ("DELETE", [bucketName], _) -> case mUser of
      Left err -> pure err
      Right user -> handleDeleteBucket store user (BucketName bucketName)

    -- HEAD /{bucket} → HeadBucket
    ("HEAD", [bucketName], _) -> case mUser of
      Left err -> pure err
      Right user -> handleHeadBucket store user (BucketName bucketName)

    -- GET /{bucket} → ListObjects
    ("GET", [bucketName], _) -> case mUser of
      Left err -> pure err
      Right user -> do
        let prefix    = lookupQuery "prefix" query
            delimiter = lookupQuery "delimiter" query
            maxKeys   = lookupQuery "max-keys" query >>= readMaybe . T.unpack
        handleListObjects store user (BucketName bucketName) prefix delimiter maxKeys

    -- PUT /{bucket}/{key} → PutObject (or UploadPart if ?uploadId present)
    ("PUT", bucketName : keyParts, _) | not (null keyParts) -> case mUser of
      Left err -> pure err
      Right user ->
        if isJust (lookupQueryBytes "uploadId" query)
          then do
            let pnM = lookupQuery "partNumber" query >>= readMaybe . T.unpack
                uidM = fmap UploadId (lookupQuery "uploadId" query)
            case (pnM, uidM) of
              (Just pn, Just uid) ->
                handleUploadPart store dataDir user uid (PartNumber pn) (sourceRequestBody req)
              _ -> pure $ errorResponse status400 "InvalidArgument" "Invalid partNumber or uploadId"
          else do
            let contentType = lookup "Content-Type" (requestHeaders req)
            let key = ObjectKey $ T.intercalate "/" keyParts
            (sha256, size, etag) <- putObject dataDir (sourceRequestBody req)
            _ <- putObjectMeta store (BucketName bucketName) key sha256 size
                  (fmap (TE.decodeUtf8With TEE.lenientDecode) contentType) [] etag
            pure $ responseLBS status200 [] ""

    -- GET /{bucket}/{key} → GetObject
    ("GET", bucketName : keyParts, _) | not (null keyParts) -> case mUser of
      Left err -> pure err
      Right user -> do
        let key = ObjectKey $ T.intercalate "/" keyParts
        handleGetObject store dataDir user (BucketName bucketName) key

    -- DELETE /{bucket}/{key} → DeleteObject (or AbortMultipartUpload if ?uploadId present)
    ("DELETE", bucketName : keyParts, _) | not (null keyParts) ->
      case lookupQuery "uploadId" query of
        Just uidStr -> case mUser of
          Left err -> pure err
          Right user -> handleAbortMultipartUpload store dataDir user (UploadId uidStr)
        Nothing -> case mUser of
          Left err -> pure err
          Right user -> do
            let key = ObjectKey $ T.intercalate "/" keyParts
            handleDeleteObject store dataDir user (BucketName bucketName) key

    -- HEAD /{bucket}/{key} → HeadObject
    ("HEAD", bucketName : keyParts, _) | not (null keyParts) -> case mUser of
      Left err -> pure err
      Right user -> do
        let key = ObjectKey $ T.intercalate "/" keyParts
        handleHeadObject store user (BucketName bucketName) key

    -- POST /{bucket}/{key}?uploads → CreateMultipartUpload
    ("POST", bucketName : keyParts, _)
      | not (null keyParts)
      , lookupQueryBytes "uploads" query == Just "" -> case mUser of
          Left err -> pure err
          Right user -> do
            let key = ObjectKey $ T.intercalate "/" keyParts
            handleCreateMultipartUpload store dataDir user (BucketName bucketName) key

    -- POST /{bucket}/{key}?uploadId=ID → CompleteMultipartUpload
    ("POST", bucketName : keyParts, _)
      | not (null keyParts)
      , Just uploadIdStr <- lookupQuery "uploadId" query -> case mUser of
          Left err -> pure err
          Right user -> do
            body <- liftIO $ lazyRequestBody req
            handleCompleteMultipartUpload store dataDir user (UploadId uploadIdStr) body

    _ -> pure $ errorResponse status405 "MethodNotAllowed" "The specified method is not allowed against this resource"

  respond result

-- | Look up a query parameter as Text (handles WAI's nested Maybe).
lookupQuery :: ByteString -> Query -> Maybe Text
lookupQuery key q = do
  mv <- join (lookup key q)
  either (const Nothing) Just $ TE.decodeUtf8' mv

-- | Look up a query parameter as raw ByteString (for comparisons like "").
lookupQueryBytes :: ByteString -> Query -> Maybe ByteString
lookupQueryBytes key q = join (lookup key q)

-- | Turn a WAI request body into a conduit source.
sourceRequestBody :: MonadIO m => Request -> ConduitT () ByteString m ()
sourceRequestBody req = do
  chunk <- liftIO $ requestBody req
  unless (B.null chunk) $ do
    yield chunk
    sourceRequestBody req

-- | Read request body as lazy ByteString.
lazyRequestBody :: Request -> IO BL.ByteString
lazyRequestBody req = do
  chunks <- runConduit $ sourceRequestBody req .| CL.consume
  pure $ BL.fromChunks chunks

-- | Start the server.
startServer :: ResolvedConfig -> IO ()
startServer config = do
  let serverCfg = rcServer config
      dataDir   = stDataDir $ rcStorage config

  store <- initStore dataDir
  waiApp <- mkApp config store

  putStrLn $ "s3-oss starting on " <> T.unpack (scHost serverCfg) <> ":" <> show (scPort serverCfg)

  let settings = setPort (scPort serverCfg)
               $ setHost (fromString $ T.unpack $ scHost serverCfg)
               $ defaultSettings

  case (scTlsCert serverCfg, scTlsKey serverCfg) of
    (Just cert, Just key) -> do
      let tls = tlsSettings cert key
      runTLS tls settings waiApp
    _ -> do
      putStrLn "WARNING: Running without TLS (development mode)"
      runSettings settings waiApp

-- | Generic error response.
errorResponse :: Status -> Text -> Text -> Response
errorResponse status code message =
  responseLBS status
    [("Content-Type", "application/xml")]
    (renderLBS $ renderError code message)
