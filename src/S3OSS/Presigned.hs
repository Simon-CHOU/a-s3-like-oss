{-# LANGUAGE OverloadedStrings #-}

-- | Presigned URL generation and validation.
module S3OSS.Presigned
  ( presignUrl
  , validatePresignedUrl
  ) where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Network.HTTP.Types (urlEncode)
import Network.Wai (Request, queryString, pathInfo, requestMethod)

-- | Percent-encode a path segment for use in a URL (RFC 3986).
encodeSegment :: Text -> Text
encodeSegment = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8

-- | Generate a presigned URL for GetObject or PutObject.
--
-- Returns 'Right' with the presigned URL on success, or 'Left' with an error
-- message if the action is unsupported or the expiry is non-positive.
presignUrl :: User -> Action -> BucketName -> ObjectKey -> NominalDiffTime -> Text -> IO (Either Text Text)
presignUrl user action bucket key expirySeconds host
  | expirySeconds <= 0 = pure $ Left "presignUrl: expirySeconds must be positive"
  | otherwise = case action of
      S3GetObject -> go "GET"
      S3PutObject -> go "PUT"
      _           -> pure $ Left $ "presignUrl: unsupported action: " <> tshow action
  where
    go :: Text -> IO (Either Text Text)
    go verb = do
      now <- getCurrentTime
      let expires = addUTCTime expirySeconds now
          expiresUnix = floor (utcTimeToPOSIXSeconds expires) :: Int
          resource = "/" <> encodeSegment (unBucketName bucket) <> "/" <> encodeSegment (unObjectKey key)
          stringToSign = T.intercalate "\n"
            [ verb
            , ""
            , ""
            , tshow expiresUnix
            , resource
            ]
          sig = hmacGetDigest $ (hmac (unSecretKey $ userSecretKey user) (TE.encodeUtf8 stringToSign) :: HMAC SHA256)
          -- Digest SHA256's Show instance produces lowercase hex
          sigHex = T.pack $ show sig
      pure $ Right $ "https://" <> host <> resource
          <> "?Credential=" <> unAccessKey (userAccessKey user)
          <> "&Expires=" <> tshow expiresUnix
          <> "&Signature=" <> sigHex

-- | Validate a presigned URL request.
--
-- Extracts Credential, Expires, and Signature from the query string,
-- finds the user by access key, checks the expiry, determines the
-- action from the HTTP method, reconstructs the string-to-sign, and
-- verifies the HMAC-SHA256 signature. Returns 'Right' with the user,
-- action, bucket and object key on success, or 'Left' with an error
-- message on failure.
validatePresignedUrl :: [User] -> Request -> IO (Either Text (User, Action, BucketName, ObjectKey))
validatePresignedUrl users req = do
  let query = queryString req
      credM = join $ lookup "Credential" query
      expM  = join $ lookup "Expires" query
      sigM  = join $ lookup "Signature" query
  case (credM, expM, sigM) of
    (Just credBS, Just expBS, Just sigBS) -> do
      case TE.decodeUtf8' credBS of
        Left _ -> pure $ Left "Invalid Credential encoding in presigned URL"
        Right credText -> case find (\u -> unAccessKey (userAccessKey u) == credText) users of
          Nothing -> pure $ Left "Invalid access key in presigned URL"
          Just user -> do
            now <- getCurrentTime
            case TE.decodeUtf8' expBS of
              Left _ -> pure $ Left "Invalid Expires encoding in presigned URL"
              Right expText -> case readMaybe (T.unpack expText) of
                Nothing -> pure $ Left "Invalid expires timestamp in presigned URL"
                Just (expUnix :: Int) -> do
                  let expTime = posixSecondsToUTCTime (fromIntegral expUnix)
                  if now > expTime
                    then pure $ Left "Presigned URL has expired"
                    else do
                      let path = pathInfo req
                      case path of
                        (bucketText : keyParts) | not (null keyParts) -> do
                          let bucketName = BucketName bucketText
                              objKey = ObjectKey $ T.intercalate "/" keyParts
                              methodBS = requestMethod req
                          case methodBS of
                            "GET" -> verifySig user bucketName objKey "GET" expText sigBS
                            "PUT" -> verifySig user bucketName objKey "PUT" expText sigBS
                            _     -> pure $ Left "Unsupported HTTP method for presigned URL"
                        _ -> pure $ Left "Invalid path in presigned URL"
    _ -> pure $ Left "Missing presigned URL parameters (Credential, Expires, Signature)"
  where
    verifySig :: User -> BucketName -> ObjectKey -> Text -> Text -> ByteString
              -> IO (Either Text (User, Action, BucketName, ObjectKey))
    verifySig user bucket key verb expiresStr sigBS = do
      let resource = "/" <> encodeSegment (unBucketName bucket) <> "/" <> encodeSegment (unObjectKey key)
          stringToSign = T.intercalate "\n"
            [ verb
            , ""
            , ""
            , expiresStr
            , resource
            ]
          computedSig = hmacGetDigest $ (hmac (unSecretKey $ userSecretKey user) (TE.encodeUtf8 stringToSign) :: HMAC SHA256)
          -- Digest SHA256's Show instance produces lowercase hex
          computedSigHex = T.pack $ show computedSig
          action = case verb of
            "GET" -> S3GetObject
            _     -> S3PutObject
      if TE.encodeUtf8 computedSigHex == sigBS
        then pure $ Right (user, action, bucket, key)
        else pure $ Left "Signature mismatch in presigned URL"
