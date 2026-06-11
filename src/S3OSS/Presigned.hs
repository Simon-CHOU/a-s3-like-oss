{-# LANGUAGE OverloadedStrings #-}

-- | Presigned URL generation and validation.
module S3OSS.Presigned where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Network.HTTP.Types (urlEncode)

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
    -- | Percent-encode a path segment for use in a URL (RFC 3986).
    encodeSegment :: Text -> Text
    encodeSegment = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8

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
