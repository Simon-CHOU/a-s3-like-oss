{-# LANGUAGE OverloadedStrings #-}

-- | Content-addressable filesystem object storage engine.
module S3OSS.Object.Storage where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import Crypto.Hash (SHA256, MD5, Context, hashInit, hashUpdate, hashFinalize, Digest)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, await, yield, (.|), transPipe, runConduitRes)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.IO (IOMode(..), withFile)
import UnliftIO.Resource (ResourceT)
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist, renameFile)

-- | Write object from a conduit source, store as content-addressed file.
-- Returns the SHA-256 hash, total size, and ETag bytes.
putObject :: FilePath -> ConduitT () ByteString IO () -> IO (Sha256Hex, Int64, ETag)
putObject dataDir source = do
  let objectsDir = dataDir <> "/objects"
  createDirectoryIfMissing True objectsDir

  -- Use a unique temp file name
  let tmpPath = objectsDir <> "/.tmp-upload"

  -- Stream source into temp file, computing SHA-256 and MD5
  (sha256Ctx, md5Ctx, totalSize) <- System.IO.withFile tmpPath WriteMode $ \h ->
    runConduitRes $ transPipe liftIO source .| transPipe liftIO (foldHashAndWrite h)

  let sha256Digest = hashFinalize sha256Ctx
  let md5Digest = hashFinalize md5Ctx
  let sha256Hex = Sha256Hex $ T.pack $ show sha256Digest
  let md5Text = T.pack $ show md5Digest
  let etag = ETag $ "\"" <> md5Text <> "\""

  -- Move to final content-addressed location
  let prefix = T.take 2 (unSha256Hex sha256Hex)
  let shardDir = objectsDir <> "/" <> T.unpack prefix
  createDirectoryIfMissing True shardDir
  let finalPath = shardDir <> "/" <> T.unpack (unSha256Hex sha256Hex)

  -- Only move if file doesn't already exist (dedup)
  exists <- doesFileExist finalPath
  if exists
    then removeFile tmpPath
    else renameFile tmpPath finalPath

  pure (sha256Hex, totalSize, etag)

-- | Read object by SHA-256 hash, returns a conduit source streaming bytes.
getObject :: FilePath -> Sha256Hex -> ConduitT () ByteString (ResourceT IO) ()
getObject dataDir sha256 = do
  let prefix = T.take 2 (unSha256Hex sha256)
  let path = dataDir <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack (unSha256Hex sha256)
  CB.sourceFile path

-- | Delete an object file from disk.
deleteObject :: FilePath -> Sha256Hex -> IO ()
deleteObject dataDir sha256 = do
  let prefix = T.take 2 (unSha256Hex sha256)
  let path = dataDir <> "/objects/" <> T.unpack prefix <> "/" <> T.unpack (unSha256Hex sha256)
  exists <- doesFileExist path
  when exists $ removeFile path

-- | Fold conduit bytes into hash contexts while writing to a file handle.
foldHashAndWrite :: Handle -> ConduitT ByteString Void IO (Context SHA256, Context MD5, Int64)
foldHashAndWrite h = loop hashInit hashInit 0
  where
    loop shaCtx md5Ctx total = do
      mbs <- await
      case mbs of
        Nothing -> pure (shaCtx, md5Ctx, total)
        Just bs -> do
          liftIO $ B.hPut h bs
          let shaCtx' = hashUpdate shaCtx bs
          let md5Ctx' = hashUpdate md5Ctx bs
          loop shaCtx' md5Ctx' (total + fromIntegral (B.length bs))
