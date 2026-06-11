{-# LANGUAGE OverloadedStrings #-}

-- | Presigned URL generation and validation.
module S3OSS.Presigned where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Time (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteArray as BA

-- | Generate a presigned URL for GetObject or PutObject.
presignUrl :: User -> Action -> BucketName -> ObjectKey -> NominalDiffTime -> Text -> IO Text
presignUrl user action bucket key expirySeconds host = do
  now <- getCurrentTime
  let expires = addUTCTime expirySeconds now
      expiresUnix = floor (utcTimeToPOSIXSeconds expires) :: Int
      verb = case action of
               S3GetObject -> "GET"
               S3PutObject -> "PUT"
               _           -> "GET"
      resource = "/" <> unBucketName bucket <> "/" <> unObjectKey key
      stringToSign = T.intercalate "\n"
        [ verb
        , ""
        , ""
        , tshow expiresUnix
        , resource
        ]
      sig = hmacGetDigest $ (hmac (unSecretKey $ userSecretKey user) (TE.encodeUtf8 stringToSign) :: HMAC SHA256)
      sigHex = T.pack $ show (BA.convert sig :: ByteString)
  pure $ "https://" <> host <> resource
      <> "?X-Amz-Algorithm=AWS4-HMAC-SHA256"
      <> "&X-Amz-Credential=" <> unAccessKey (userAccessKey user)
      <> "&X-Amz-Expires=" <> tshow expiresUnix
      <> "&X-Amz-Signature=" <> sigHex
