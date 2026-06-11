{-# LANGUAGE OverloadedStrings #-}

-- | AWS Signature Version 4 (SigV4) verification.
module S3OSS.Auth.SigV4
  ( deriveSigningKey
  , AuthHeader(..)
  , verifySigV4
  , buildStringToSign
  , hashCanonicalRequest
  , buildCanonicalRequest
  , parseAuthHeader
  ) where

import RIO
import S3OSS.Types
import qualified RIO.Text as T
import qualified Data.Text.Encoding as TE
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Hash (Digest)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import Network.Wai (Request, requestHeaders, requestMethod, rawPathInfo, rawQueryString)
import Data.CaseInsensitive (original)
import qualified Data.Text as DT
import Data.List (find, sortBy)

-- | Derive the AWS SigV4 signing key.
deriveSigningKey :: SecretKey -> Text -> Text -> Text -> ByteString
deriveSigningKey (SecretKey secret) date region service =
  let kDate     = hmacGetDigest $ (hmac ("AWS4" <> secret) (TE.encodeUtf8 date) :: HMAC SHA256)
      kRegion   = hmacGetDigest $ (hmac kDate (TE.encodeUtf8 region) :: HMAC SHA256)
      kService  = hmacGetDigest $ (hmac kRegion (TE.encodeUtf8 service) :: HMAC SHA256)
      kSigning  = hmacGetDigest $ (hmac kService ("aws4_request" :: ByteString) :: HMAC SHA256)
  in BA.convert kSigning

-- | Parsed Authorization header components.
data AuthHeader = AuthHeader
  { ahAccessKey    :: Text
  , ahDate         :: Text
  , ahRegion       :: Text
  , ahService      :: Text
  , ahSignedHeaders :: [Text]
  , ahSignature    :: ByteString
  }

-- | Verify a SigV4-signed request against a user's secret key.
-- In development mode, returns the first user unconditionally.
verifySigV4 :: Bool -> [User] -> Request -> IO (Either Text User)
verifySigV4 devMode users req
  | devMode = case users of
      (u:_) -> pure $ Right u
      []    -> pure $ Left "No users configured"
  | otherwise = do
      let mAuthHeader = lookup "Authorization" (requestHeaders req)
      case mAuthHeader of
        Nothing -> pure $ Left "Missing Authorization header"
        Just authBs -> do
          case parseAuthHeader $ TE.decodeUtf8With lenientDecode authBs of
            Left err -> pure $ Left err
            Right ah -> do
              case find (\u -> unAccessKey (userAccessKey u) == ahAccessKey ah) users of
                Nothing -> pure $ Left "Invalid access key"
                Just user -> do
                  -- Extract the full ISO 8601 timestamp from X-Amz-Date header
                  let mAmzDate = lookup "X-Amz-Date" (requestHeaders req)
                  case mAmzDate of
                    Nothing -> pure $ Left "Missing X-Amz-Date header"
                    Just amzDateBs -> do
                      let amzDate = TE.decodeUtf8With lenientDecode amzDateBs
                      -- Build string-to-sign and verify
                      let stringToSign = buildStringToSign amzDate req ah
                          signingKey = deriveSigningKey (userSecretKey user) (ahDate ah) (ahRegion ah) (ahService ah)
                          expected = hmacGetDigest $ (hmac signingKey (TE.encodeUtf8 stringToSign) :: HMAC SHA256)
                      -- Hex-encode the expected digest for comparison (both sides must be hex-encoded ASCII)
                      let expectedHex = TE.encodeUtf8 (T.pack (show expected))
                      if ahSignature ah == expectedHex
                        then pure $ Right user
                        else pure $ Left "Signature mismatch"

-- | Build the SigV4 string-to-sign.
buildStringToSign :: Text -> Request -> AuthHeader -> Text
buildStringToSign amzDate req ah =
  T.intercalate "\n"
    [ "AWS4-HMAC-SHA256"
    , amzDate
    , ahDate ah <> "/" <> ahRegion ah <> "/" <> ahService ah <> "/aws4_request"
    , hashCanonicalRequest req ah
    ]

-- | Compute SHA-256 hash of the canonical request.
hashCanonicalRequest :: Request -> AuthHeader -> Text
hashCanonicalRequest req ah = do
  let cr = buildCanonicalRequest req ah
  let digest = Crypto.hash (TE.encodeUtf8 cr) :: Digest SHA256
  T.pack $ show digest

-- | Build the canonical request string.
buildCanonicalRequest :: Request -> AuthHeader -> Text
buildCanonicalRequest req ah =
  let method = decodeUtf8With lenientDecode (requestMethod req)
      uri = decodeUtf8With lenientDecode (rawPathInfo req)
      query = decodeUtf8With lenientDecode (rawQueryString req)

      signedHeaderNames = ahSignedHeaders ah
      allHeaders = requestHeaders req

      -- Filter to only the headers listed in SignedHeaders
      relevantHeaders = filter (\(k, _) ->
        let name = T.toLower (decodeUtf8With lenientDecode (original k))
        in name `elem` signedHeaderNames
        ) allHeaders

      -- Sort lexicographically by lowercase header name
      sortedHeaders = sortBy
        (\(k1, _) (k2, _) ->
          compare (T.toLower (decodeUtf8With lenientDecode (original k1)))
                  (T.toLower (decodeUtf8With lenientDecode (original k2)))
        ) relevantHeaders

      -- Format as lowercase-header-name:header-value\n
      headerLines = map (\(k, v) ->
        T.toLower (decodeUtf8With lenientDecode (original k)) <> ":"
        <> T.strip (decodeUtf8With lenientDecode v)
        ) sortedHeaders
      headers = T.intercalate "\n" headerLines

      signedHdrs = T.intercalate ";" signedHeaderNames
      -- Use UNSIGNED-PAYLOAD for simplicity
      payloadHash = "UNSIGNED-PAYLOAD"
  in T.intercalate "\n"
    [ method, uri, query, headers, "", signedHdrs, payloadHash ]

-- | Parse the Authorization header.
parseAuthHeader :: Text -> Either Text AuthHeader
parseAuthHeader t =
  case DT.splitOn ", " t of
    [] -> Left "Empty Authorization header"
    (algoPart : restParts) -> do
      unless ("AWS4-HMAC-SHA256" `T.isPrefixOf` algoPart) $
        Left "Expected AWS4-HMAC-SHA256 algorithm"

      -- Parse Credential
      afterAlgo <- case T.stripPrefix "AWS4-HMAC-SHA256 Credential=" algoPart of
        Just a  -> Right a
        Nothing -> Left "Missing Credential"

      case DT.splitOn "/" afterAlgo of
        [accessKey, date, region, service, _] -> do
          -- Parse SignedHeaders
          let rest = T.intercalate ", " restParts
          case T.stripPrefix "SignedHeaders=" rest of
            Nothing -> Left "Missing SignedHeaders"
            Just shPart ->
              case DT.splitOn ", Signature=" shPart of
                [signedHeadersStr, sig] -> do
                  let signedHeaders = DT.splitOn ";" signedHeadersStr
                  pure $ AuthHeader accessKey date region service signedHeaders (TE.encodeUtf8 sig)
                _ -> Left "Expected SignedHeaders, Signature"
        _ -> Left "Invalid credential scope format"
