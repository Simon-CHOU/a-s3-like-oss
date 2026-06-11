{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | SQLite metadata persistence layer.
module S3OSS.Store (module S3OSS.Store) where

import RIO
import S3OSS.Types
import Database.SQLite.Simple
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.List (partition, nub, sort)
import System.Directory (createDirectoryIfMissing)

-- | Database handle.
data Store = Store
  { storeConn :: Connection
  }

-- | Initialize the store: create tables if they don't exist.
initStore :: FilePath -> IO Store
initStore dataDir = do
  let dbPath = dataDir <> "/meta.sqlite"
  createDirectoryIfMissing True dataDir
  conn <- open dbPath
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA foreign_keys=ON"
  mapM_ (execute_ conn) schema
  pure $ Store conn

schema :: [Query]
schema =
  [ "CREATE TABLE IF NOT EXISTS buckets ("
    <> "  id         INTEGER PRIMARY KEY AUTOINCREMENT,"
    <> "  name       TEXT NOT NULL UNIQUE,"
    <> "  created_at TEXT NOT NULL"
    <> ");"
  , "CREATE TABLE IF NOT EXISTS objects ("
    <> "  id           INTEGER PRIMARY KEY AUTOINCREMENT,"
    <> "  bucket_id    INTEGER NOT NULL REFERENCES buckets(id),"
    <> "  key          TEXT NOT NULL,"
    <> "  sha256       TEXT NOT NULL,"
    <> "  size         INTEGER NOT NULL,"
    <> "  content_type TEXT,"
    <> "  etag         TEXT NOT NULL,"
    <> "  metadata     TEXT DEFAULT '[]',"
    <> "  created_at   TEXT NOT NULL,"
    <> "  updated_at   TEXT NOT NULL,"
    <> "  UNIQUE(bucket_id, key)"
    <> ");"
  , "CREATE TABLE IF NOT EXISTS multipart_uploads ("
    <> "  upload_id  TEXT PRIMARY KEY,"
    <> "  bucket_id  INTEGER NOT NULL REFERENCES buckets(id),"
    <> "  key        TEXT NOT NULL,"
    <> "  state      TEXT NOT NULL DEFAULT 'initiated',"
    <> "  created_at TEXT NOT NULL,"
    <> "  expires_at TEXT NOT NULL"
    <> ");"
  , "CREATE TABLE IF NOT EXISTS multipart_parts ("
    <> "  upload_id   TEXT NOT NULL REFERENCES multipart_uploads(upload_id),"
    <> "  part_number INTEGER NOT NULL,"
    <> "  sha256      TEXT NOT NULL,"
    <> "  size        INTEGER NOT NULL,"
    <> "  etag        TEXT NOT NULL,"
    <> "  PRIMARY KEY (upload_id, part_number)"
    <> ");"
  ]

-- Helpers

iso8601 :: UTCTime -> Text
iso8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

parseTimeIso8601 :: Text -> Maybe UTCTime
parseTimeIso8601 t =
  let s = T.unpack t
  in  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s
      <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q UTC" s

getBucketId :: Store -> BucketName -> IO (Either Text Int)
getBucketId store name = do
  rows <- query (storeConn store)
    "SELECT id FROM buckets WHERE name = ?" (Only $ unBucketName name)
  pure $ case rows of
    [Only bid] -> Right bid
    _          -> Left "BucketNotFound"

getBucketIdMaybe :: Store -> BucketName -> IO (Maybe Int)
getBucketIdMaybe store name = do
  rows <- query (storeConn store)
    "SELECT id FROM buckets WHERE name = ?" (Only $ unBucketName name)
  pure $ case rows of
    [Only bid] -> Just bid
    _          -> Nothing

-- Bucket operations

createBucket :: Store -> BucketName -> IO (Either Text BucketInfo)
createBucket store name = do
  now <- getCurrentTime
  result <- tryAny $
    execute (storeConn store)
      "INSERT INTO buckets (name, created_at) VALUES (?, ?)"
      (unBucketName name, iso8601 now)
  case result of
    Left _  -> pure $ Left "BucketAlreadyExists"
    Right _ -> pure $ Right $ BucketInfo name now

-- | Result of a bucket deletion attempt.
data BucketDeleteStatus
  = BucketDeleted
  | BucketNotFound
  | BucketNotEmpty
  deriving (Eq, Show)

deleteBucket :: Store -> BucketName -> IO BucketDeleteStatus
deleteBucket store name = do
  mBid <- getBucketIdMaybe store name
  case mBid of
    Nothing -> pure BucketNotFound
    Just bid -> do
      objCount <- query (storeConn store)
        "SELECT COUNT(*) FROM objects WHERE bucket_id = ?"
        (Only bid) :: IO [Only Int]
      if objCount == [Only (0 :: Int)]
        then do
          execute (storeConn store)
            "DELETE FROM buckets WHERE name = ?" (Only $ unBucketName name)
          pure BucketDeleted
        else pure BucketNotEmpty

listBuckets :: Store -> IO [BucketInfo]
listBuckets store = do
  rows <- query_ (storeConn store) "SELECT name, created_at FROM buckets ORDER BY name"
  pure $ mapMaybe (\(n, t) -> BucketInfo (BucketName n) <$> parseTimeIso8601 t) rows

headBucket :: Store -> BucketName -> IO Bool
headBucket store name = do
  result <- query (storeConn store)
    "SELECT 1 FROM buckets WHERE name = ?" (Only $ unBucketName name)
  pure (not (null (result :: [Only Int])))

-- Object operations

putObjectMeta :: Store -> BucketName -> ObjectKey -> Sha256Hex -> Int64 -> Maybe Text -> [(Text, Text)] -> ETag -> IO (Either Text ObjectInfo)
putObjectMeta store bucket key hash size contentType metaPairs etag = do
  now <- getCurrentTime
  eBucketId <- getBucketId store bucket
  case eBucketId of
    Left err -> pure $ Left err
    Right bucketId -> do
      let metaText = "[]" :: Text  -- simplified; full JSON encoding not needed for MVP
      execute (storeConn store)
        "INSERT INTO objects (bucket_id, key, sha256, size, content_type, etag, metadata, created_at, updated_at) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
        \ON CONFLICT(bucket_id, key) DO UPDATE SET \
        \  sha256=excluded.sha256, size=excluded.size, content_type=excluded.content_type, \
        \  etag=excluded.etag, metadata=excluded.metadata, updated_at=excluded.updated_at"
        (bucketId, unObjectKey key, unSha256Hex hash, size, contentType, unETag etag, metaText, iso8601 now, iso8601 now)
      pure $ Right $ ObjectInfo bucket key hash size contentType metaPairs etag now now

getObjectMeta :: Store -> BucketName -> ObjectKey -> IO (Maybe ObjectInfo)
getObjectMeta store bucket key = do
  rows <- query (storeConn store)
    "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
    \FROM objects o JOIN buckets b ON o.bucket_id = b.id \
    \WHERE b.name = ? AND o.key = ?"
    (unBucketName bucket, unObjectKey key)
  pure $ listToMaybe $ mapMaybe toObjectInfo rows
  where
    toObjectInfo (bn, k, h, sz, ct, e, ca, ua) = do
      ca' <- parseTimeIso8601 ca
      ua' <- parseTimeIso8601 ua
      Just $ ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e) ca' ua'

deleteObjectMeta :: Store -> BucketName -> ObjectKey -> IO Bool
deleteObjectMeta store bucket key = do
  mBid <- getBucketIdMaybe store bucket
  case mBid of
    Nothing -> pure False
    Just bucketId -> do
      execute (storeConn store)
        "DELETE FROM objects WHERE bucket_id = ? AND key = ?"
        (bucketId, unObjectKey key)
      rows <- query_ (storeConn store) "SELECT changes()" :: IO [Only Int]
      let rowCount = case rows of
            [Only c] -> c
            _        -> 0
      pure (rowCount > 0)

listObjects :: Store -> BucketName -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> IO ([ObjectInfo], [Text])
listObjects store bucket prefix delimiter marker maxKeys = do
  mBid <- getBucketIdMaybe store bucket
  case mBid of
    Nothing -> pure ([], [])
    Just bucketId -> do
      let limit = maxKeys + 1
      rows <- fetchRows bucketId prefix marker limit
      let objects = mapMaybe toObj rows
      case delimiter of
        Nothing -> pure (take maxKeys objects, [])
        Just d  -> pure $ groupByDelimiter prefix d objects maxKeys
  where
    fetchRows bid mPrefix mMarker lim = case (mPrefix, mMarker) of
      (Just p, Just m) -> query (storeConn store)
        "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
        \FROM objects o JOIN buckets b ON o.bucket_id = b.id \
        \WHERE b.id = ? AND o.key LIKE ? AND o.key > ? \
        \ORDER BY o.key LIMIT ?"
        (bid, p <> "%", m, lim) :: IO [(Text, Text, Text, Int64, Maybe Text, Text, Text, Text)]
      (Just p, Nothing) -> query (storeConn store)
        "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
        \FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.id = ? AND o.key LIKE ? \
        \ORDER BY o.key LIMIT ?"
        (bid, p <> "%", lim) :: IO [(Text, Text, Text, Int64, Maybe Text, Text, Text, Text)]
      (Nothing, Just m) -> query (storeConn store)
        "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
        \FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.id = ? AND o.key > ? \
        \ORDER BY o.key LIMIT ?"
        (bid, m, lim) :: IO [(Text, Text, Text, Int64, Maybe Text, Text, Text, Text)]
      (Nothing, Nothing) -> query (storeConn store)
        "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
        \FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.id = ? \
        \ORDER BY o.key LIMIT ?"
        (bid, lim) :: IO [(Text, Text, Text, Int64, Maybe Text, Text, Text, Text)]

    toObj (bn, k, h, sz, ct, e, ca, ua) = do
      ca' <- parseTimeIso8601 ca
      ua' <- parseTimeIso8601 ua
      Just $ ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e) ca' ua'

    -- | Group objects by delimiter: keys containing the delimiter after the prefix
    -- become CommonPrefixes, others remain as regular object entries.
    groupByDelimiter mPrefix d objs limit =
      let (dirLike, fileLike) = partition (hasDelimiterAfterPrefix mPrefix d . unObjectKey . oiKey) objs
          commonPrefs = nub $ sort $ mapMaybe (extractCommonPrefix mPrefix d . unObjectKey . oiKey) dirLike
          takeFiles = take limit fileLike
          remaining = limit - length takeFiles
          takePrefs = take remaining commonPrefs
      in (takeFiles, takePrefs)

    hasDelimiterAfterPrefix mPrefix d key =
      let stripped = case mPrefix of
            Just p  -> fromMaybe key (T.stripPrefix p key)
            Nothing -> key
      in d `T.isInfixOf` stripped

    extractCommonPrefix mPrefix d key =
      let stripped = case mPrefix of
            Just p  -> fromMaybe key (T.stripPrefix p key)
            Nothing -> key
      in case T.breakOn d stripped of
           (_, "") -> Nothing
           (part, _) -> Just (fromMaybe "" mPrefix <> part <> d)

-- Multipart operations

createMultipartUpload :: Store -> BucketName -> ObjectKey -> UploadId -> IO (Either Text MultipartUpload)
createMultipartUpload store bucket key uploadId = do
  now <- getCurrentTime
  let expiresAt = addUTCTime (7 * 24 * 60 * 60) now
  eBucketId <- getBucketId store bucket
  case eBucketId of
    Left err -> pure $ Left err
    Right bucketId -> do
      execute (storeConn store)
        "INSERT INTO multipart_uploads (upload_id, bucket_id, key, state, created_at, expires_at) VALUES (?, ?, ?, 'initiated', ?, ?)"
        (unUploadId uploadId, bucketId, unObjectKey key, iso8601 now, iso8601 expiresAt)
      pure $ Right $ MultipartUpload uploadId bucket key UploadInitiated now expiresAt

addPart :: Store -> UploadId -> PartNumber -> Sha256Hex -> Int64 -> ETag -> IO ()
addPart store uploadId partNum hash size etag = do
  execute (storeConn store)
    "INSERT OR REPLACE INTO multipart_parts (upload_id, part_number, sha256, size, etag) VALUES (?, ?, ?, ?, ?)"
    (unUploadId uploadId, unPartNumber partNum, unSha256Hex hash, size, unETag etag)

getParts :: Store -> UploadId -> IO [PartInfo]
getParts store uploadId = do
  rows <- query (storeConn store)
    "SELECT part_number, sha256, size, etag FROM multipart_parts WHERE upload_id = ? ORDER BY part_number"
    (Only $ unUploadId uploadId)
  pure [PartInfo (PartNumber pn) (Sha256Hex h) sz (ETag e) | (pn, h, sz, e) <- rows]

getMultipartUpload :: Store -> UploadId -> IO (Maybe MultipartUpload)
getMultipartUpload store uploadId = do
  rows <- query (storeConn store)
    "SELECT m.upload_id, b.name, m.key, m.state, m.created_at, m.expires_at \
    \FROM multipart_uploads m JOIN buckets b ON m.bucket_id = b.id \
    \WHERE m.upload_id = ? AND m.state = 'initiated'"
    (Only $ unUploadId uploadId) :: IO [(Text, Text, Text, Text, Text, Text)]
  pure $ listToMaybe $ mapMaybe toUpload rows
  where
    toUpload (uid, bn, k, st, ca, ea) = do
      ca' <- parseTimeIso8601 ca
      ea' <- parseTimeIso8601 ea
      Just $ MultipartUpload (UploadId uid) (BucketName bn) (ObjectKey k)
        (case st of "initiated" -> UploadInitiated; "completed" -> UploadCompleted; _ -> UploadAborted)
        ca' ea'

completeMultipartUpload :: Store -> UploadId -> IO ()
completeMultipartUpload store uploadId = do
  execute (storeConn store)
    "UPDATE multipart_uploads SET state = 'completed' WHERE upload_id = ?"
    (Only $ unUploadId uploadId)

abortMultipartUpload :: Store -> UploadId -> IO ()
abortMultipartUpload store uploadId = do
  execute (storeConn store)
    "DELETE FROM multipart_parts WHERE upload_id = ?" (Only $ unUploadId uploadId)
  execute (storeConn store)
    "DELETE FROM multipart_uploads WHERE upload_id = ?" (Only $ unUploadId uploadId)

cleanupExpiredUploads :: Store -> IO Int
cleanupExpiredUploads store = do
  now <- getCurrentTime
  expired <- query (storeConn store)
    "SELECT upload_id FROM multipart_uploads WHERE state = 'initiated' AND expires_at < ?"
    (Only $ iso8601 now)
  forM_ expired $ \(Only uid) -> abortMultipartUpload store (UploadId uid)
  pure (length expired)
