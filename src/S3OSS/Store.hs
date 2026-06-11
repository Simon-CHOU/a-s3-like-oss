{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | SQLite metadata persistence layer.
module S3OSS.Store where

import RIO
import S3OSS.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
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
  execute_ conn (Query schema)
  pure $ Store conn

schema :: T.Text
schema = T.intercalate "\n"
  [ "CREATE TABLE IF NOT EXISTS buckets ("
  , "  id         INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "  name       TEXT NOT NULL UNIQUE,"
  , "  created_at TEXT NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS objects ("
  , "  id           INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "  bucket_id    INTEGER NOT NULL REFERENCES buckets(id),"
  , "  key          TEXT NOT NULL,"
  , "  sha256       TEXT NOT NULL,"
  , "  size         INTEGER NOT NULL,"
  , "  content_type TEXT,"
  , "  etag         TEXT NOT NULL,"
  , "  metadata     TEXT DEFAULT '[]',"
  , "  ref_count    INTEGER NOT NULL DEFAULT 1,"
  , "  created_at   TEXT NOT NULL,"
  , "  updated_at   TEXT NOT NULL,"
  , "  UNIQUE(bucket_id, key)"
  , ");"
  , "CREATE TABLE IF NOT EXISTS multipart_uploads ("
  , "  upload_id  TEXT PRIMARY KEY,"
  , "  bucket_id  INTEGER NOT NULL REFERENCES buckets(id),"
  , "  key        TEXT NOT NULL,"
  , "  state      TEXT NOT NULL DEFAULT 'initiated',"
  , "  created_at TEXT NOT NULL,"
  , "  expires_at TEXT NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS multipart_parts ("
  , "  upload_id   TEXT NOT NULL REFERENCES multipart_uploads(upload_id),"
  , "  part_number INTEGER NOT NULL,"
  , "  sha256      TEXT NOT NULL,"
  , "  size        INTEGER NOT NULL,"
  , "  etag        TEXT NOT NULL,"
  , "  PRIMARY KEY (upload_id, part_number)"
  , ");"
  ]

-- Helpers

iso8601 :: UTCTime -> Text
iso8601 = tshow

parseTimeIso8601 :: Text -> UTCTime
parseTimeIso8601 t =
  case readMaybe (T.unpack t) of
    Just utc -> utc
    Nothing  -> error $ "Invalid ISO-8601 timestamp: " <> T.unpack t

getBucketId :: Store -> BucketName -> IO Int
getBucketId store name = do
  rows <- query (storeConn store)
    "SELECT id FROM buckets WHERE name = ?" (Only $ unBucketName name)
  case rows of
    [Only bid] -> pure bid
    _ -> error $ "Bucket not found: " <> T.unpack (unBucketName name)

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

deleteBucket :: Store -> BucketName -> IO Bool
deleteBucket store name = do
  objCount <- query (storeConn store)
    "SELECT COUNT(*) FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.name = ?"
    (Only $ unBucketName name) :: IO [Only Int]
  if objCount == [Only (0 :: Int)]
    then do
      execute (storeConn store)
        "DELETE FROM buckets WHERE name = ?" (Only $ unBucketName name)
      pure True
    else pure False

listBuckets :: Store -> IO [BucketInfo]
listBuckets store = do
  rows <- query_ (storeConn store) "SELECT name, created_at FROM buckets ORDER BY name"
  pure [BucketInfo (BucketName n) (parseTimeIso8601 t) | (n, t) <- rows]

headBucket :: Store -> BucketName -> IO Bool
headBucket store name = do
  result <- query (storeConn store)
    "SELECT 1 FROM buckets WHERE name = ?" (Only $ unBucketName name)
  pure (not (null (result :: [Only Int])))

-- Object operations

putObjectMeta :: Store -> BucketName -> ObjectKey -> Sha256Hex -> Int64 -> Maybe Text -> [(Text, Text)] -> ETag -> IO ObjectInfo
putObjectMeta store bucket key hash size contentType metaPairs etag = do
  now <- getCurrentTime
  bucketId <- getBucketId store bucket
  let metaText = "[]" :: Text  -- simplified; full JSON encoding not needed for MVP
  execute (storeConn store)
    "INSERT INTO objects (bucket_id, key, sha256, size, content_type, etag, metadata, created_at, updated_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(bucket_id, key) DO UPDATE SET \
    \  sha256=excluded.sha256, size=excluded.size, content_type=excluded.content_type, \
    \  etag=excluded.etag, metadata=excluded.metadata, updated_at=excluded.updated_at"
    (bucketId, unObjectKey key, unSha256Hex hash, size, contentType, unETag etag, metaText, iso8601 now, iso8601 now)
  pure $ ObjectInfo bucket key hash size contentType metaPairs etag now now

getObjectMeta :: Store -> BucketName -> ObjectKey -> IO (Maybe ObjectInfo)
getObjectMeta store bucket key = do
  rows <- query (storeConn store)
    "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
    \FROM objects o JOIN buckets b ON o.bucket_id = b.id \
    \WHERE b.name = ? AND o.key = ?"
    (unBucketName bucket, unObjectKey key)
  pure $ listToMaybe $ map toObjectInfo rows
  where
    toObjectInfo (bn, k, h, sz, ct, e, ca, ua) =
      ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e)
        (parseTimeIso8601 ca) (parseTimeIso8601 ua)

deleteObjectMeta :: Store -> BucketName -> ObjectKey -> IO Bool
deleteObjectMeta store bucket key = do
  mBid <- getBucketIdMaybe store bucket
  case mBid of
    Nothing -> pure False
    Just bucketId -> do
      execute (storeConn store)
        "DELETE FROM objects WHERE bucket_id = ? AND key = ?"
        (bucketId, unObjectKey key)
      [Only changes] <- query_ (storeConn store) "SELECT changes()" :: IO [Only Int]
      pure (changes > 0)

listObjects :: Store -> BucketName -> Maybe Text -> Maybe Text -> Int -> IO [ObjectInfo]
listObjects store bucket prefix _delimiter maxKeys = do
  mBid <- getBucketIdMaybe store bucket
  case mBid of
    Nothing -> pure []
    Just bucketId -> do
      let baseQuery = "SELECT b.name, o.key, o.sha256, o.size, o.content_type, o.etag, o.created_at, o.updated_at \
                      \FROM objects o JOIN buckets b ON o.bucket_id = b.id WHERE b.id = ?"
          (prefixClause, params) = case prefix of
            Just p  -> (" AND o.key LIKE ?", [tshow bucketId, p <> "%"])
            Nothing -> ("", [tshow bucketId])
          query' = baseQuery <> prefixClause <> " ORDER BY o.key LIMIT " <> tshow (maxKeys + 1)
      rows <- query (storeConn store) (Query query') (map (\(Only t) -> t) (map Only params) :: [Text])
      pure $ map toObj rows
  where
    toObj (bn, k, h, sz, ct, e, ca, ua) =
      ObjectInfo (BucketName bn) (ObjectKey k) (Sha256Hex h) sz ct [] (ETag e)
        (parseTimeIso8601 ca) (parseTimeIso8601 ua)

-- Multipart operations

createMultipartUpload :: Store -> BucketName -> ObjectKey -> UploadId -> IO MultipartUpload
createMultipartUpload store bucket key uploadId = do
  now <- getCurrentTime
  let expiresAt = addUTCTime (7 * 24 * 60 * 60) now
  bucketId <- getBucketId store bucket
  execute (storeConn store)
    "INSERT INTO multipart_uploads (upload_id, bucket_id, key, state, created_at, expires_at) VALUES (?, ?, ?, 'initiated', ?, ?)"
    (unUploadId uploadId, bucketId, unObjectKey key, iso8601 now, iso8601 expiresAt)
  pure $ MultipartUpload uploadId bucket key UploadInitiated now expiresAt

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
  pure $ listToMaybe $ map toUpload rows
  where
    toUpload (uid, bn, k, st, ca, ea) =
      MultipartUpload (UploadId uid) (BucketName bn) (ObjectKey k)
        (case st of "initiated" -> UploadInitiated; "completed" -> UploadCompleted; _ -> UploadAborted)
        (parseTimeIso8601 ca) (parseTimeIso8601 ea)

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

instance ToField BucketName where toField = toField . unBucketName
instance FromField BucketName where fromField f = BucketName <$> fromField f
instance ToField ObjectKey where toField = toField . unObjectKey
instance FromField ObjectKey where fromField f = ObjectKey <$> fromField f
