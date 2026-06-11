{-# LANGUAGE OverloadedStrings #-}

module S3OSS.Auth.SigV4Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import S3OSS.Auth.SigV4
import S3OSS.Types
import Network.Wai (Request, defaultRequest, requestMethod, requestHeaders, rawPathInfo, rawQueryString)
import Network.HTTP.Types (methodPut)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)
import Data.Either (isLeft)
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Make a canonical request and then hash it.
hashOf :: Request -> AuthHeader -> Text
hashOf req ah = hashCanonicalRequest req ah

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -------------------------------------------------------------------
  -- 1. Signing Key Derivation
  -------------------------------------------------------------------
  describe "SigV4 Signing Key Derivation" $ do
    it "produces a non-empty signing key" $ do
      let secret  = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let date    = "20150830"
      let region  = "us-east-1"
      let service = "iam"
      let signingKey = deriveSigningKey secret date region service
      B.length signingKey `shouldSatisfy` (> 0)
      B.length signingKey `shouldBe` 32

    it "produces the expected signing key per AWS test vector" $ do
      let secret  = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let signingKey = deriveSigningKey secret "20150830" "us-east-1" "iam"
      let keyHex = T.toLower $ T.pack $ B.unpack signingKey >>= \w ->
            let s = showHex w "" in replicate (2 - length s) '0' ++ s
      keyHex `shouldBe` "969fbb94feb542b71ede6f87fe4d5fa29c789342b0f407474670f0c2489e0a0d"

    prop "always yields a 32-byte key for arbitrary secrets" $ property $
      forAll (listOf1 arbitrary `suchThat` (not . null)) $ \(bytes :: [Word8]) ->
        let key = B.pack bytes
        in B.length (deriveSigningKey (SecretKey key) "20210101" "us-east-1" "s3") == 32

  -------------------------------------------------------------------
  -- 2. Canonical Request Construction
  -------------------------------------------------------------------
  describe "Canonical Request Construction" $ do
    it "builds a minimal GET canonical request" $ do
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , rawQueryString = ""
            , requestHeaders = [("Host", "example.com"), ("X-Amz-Date", "20150830T123600Z")]
            }
      let ah = AuthHeader "" "" "" "" ["host", "x-amz-date"] ""
      let cr = buildCanonicalRequest req ah
      let expected = T.intercalate "\n"
            [ "GET"
            , "/"
            , ""
            , "host:example.com"
            , "x-amz-date:20150830T123600Z"
            , ""
            , "host;x-amz-date"
            , "UNSIGNED-PAYLOAD"
            ]
      cr `shouldBe` expected

    it "sorts headers lexicographically by lowercase header name" $ do
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , requestHeaders =
                [ ("X-Amz-Date"   , "20150830T123600Z")
                , ("Content-Type" , "application/json")
                , ("Host"         , "example.com")
                ]
            }
      let ah = AuthHeader "" "" "" "" ["content-type", "host", "x-amz-date"] ""
      let cr = buildCanonicalRequest req ah
      let expected = T.intercalate "\n"
            [ "GET"
            , "/"
            , ""
            , "content-type:application/json"
            , "host:example.com"
            , "x-amz-date:20150830T123600Z"
            , ""
            , "content-type;host;x-amz-date"
            , "UNSIGNED-PAYLOAD"
            ]
      cr `shouldBe` expected

    it "includes the query string in canonical request" $ do
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/test.txt"
            , rawQueryString = "?X-Amz-Algorithm=AWS4-HMAC-SHA256"
            , requestHeaders = [("Host", "example.com")]
            }
      let ah = AuthHeader "" "" "" "" ["host"] ""
      let cr = buildCanonicalRequest req ah
      T.lines cr !! 2 `shouldBe` "?X-Amz-Algorithm=AWS4-HMAC-SHA256"

    it "uses UNSIGNED-PAYLOAD as the body hash placeholder" $ do
      let req = defaultRequest
            { requestMethod  = "PUT"
            , rawPathInfo    = "/"
            , requestHeaders = [("Host", "example.com")]
            }
      let ah = AuthHeader "" "" "" "" ["host"] ""
      let cr = buildCanonicalRequest req ah
      "UNSIGNED-PAYLOAD" `T.isSuffixOf` cr `shouldBe` True

    it "strips whitespace from header values" $ do
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , requestHeaders = [("Host", "  example.com  ")]
            }
      let ah = AuthHeader "" "" "" "" ["host"] ""
      let cr = buildCanonicalRequest req ah
      T.lines cr !! 3 `shouldBe` "host:example.com"

  -------------------------------------------------------------------
  -- 3. String-to-Sign Construction
  -------------------------------------------------------------------
  describe "String-to-Sign Construction" $ do
    it "builds the string-to-sign with correct format" $ do
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , requestHeaders = [("Host", "example.com"), ("X-Amz-Date", "20150830T123600Z")]
            }
      let ah = AuthHeader "AKID" "20150830" "us-east-1" "iam" ["host", "x-amz-date"] ""
      let crHash = hashCanonicalRequest req ah
      let sts = buildStringToSign "20150830T123600Z" req ah
      let expectedScope = "20150830/us-east-1/iam/aws4_request"
      sts `shouldBe` T.intercalate "\n"
            [ "AWS4-HMAC-SHA256"
            , "20150830T123600Z"
            , expectedScope
            , crHash
            ]

  -------------------------------------------------------------------
  -- 4. Authorization Header Parsing
  -------------------------------------------------------------------
  describe "Authorization Header Parsing" $ do
    it "parses a valid Authorization header" $ do
      let header = "AWS4-HMAC-SHA256 Credential=AKID/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abcd1234"
      case parseAuthHeader header of
        Right ah -> do
          ahAccessKey ah    `shouldBe` "AKID"
          ahDate ah         `shouldBe` "20150830"
          ahRegion ah       `shouldBe` "us-east-1"
          ahService ah      `shouldBe` "iam"
          ahSignedHeaders ah `shouldBe` ["host", "x-amz-date"]
          ahSignature ah    `shouldBe` encodeUtf8 "abcd1234"
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ T.unpack err

    it "parses header with single signed header" $ do
      let header = "AWS4-HMAC-SHA256 Credential=AKID/20150830/us-east-1/s3/aws4_request, SignedHeaders=host, Signature=00010203"
      case parseAuthHeader header of
        Right ah -> do
          ahAccessKey ah    `shouldBe` "AKID"
          ahSignedHeaders ah `shouldBe` ["host"]
          ahSignature ah    `shouldBe` encodeUtf8 "00010203"
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ T.unpack err

    it "rejects empty Authorization header" $ do
      parseAuthHeader "" `shouldBe` Left "Empty Authorization header"

    it "rejects header without AWS4-HMAC-SHA256 algorithm" $ do
      parseAuthHeader "UnknownAlgo Credential=x" `shouldBe`
        Left "Expected AWS4-HMAC-SHA256 algorithm"

    it "rejects header without Credential" $ do
      parseAuthHeader "AWS4-HMAC-SHA256 NotCredential=x" `shouldBe`
        Left "Missing Credential"

    it "rejects invalid credential scope (wrong number of parts)" $ do
      parseAuthHeader "AWS4-HMAC-SHA256 Credential=AKID/only/three/parts" `shouldBe`
        Left "Invalid credential scope format"

    it "rejects header without SignedHeaders" $ do
      parseAuthHeader "AWS4-HMAC-SHA256 Credential=AKID/date/region/service/suffix, Signature=abcd" `shouldBe`
        Left "Missing SignedHeaders"

    it "rejects header without Signature" $ do
      parseAuthHeader "AWS4-HMAC-SHA256 Credential=AKID/date/region/service/suffix, SignedHeaders=host" `shouldBe`
        Left "Expected SignedHeaders, Signature"

  -------------------------------------------------------------------
  -- 5. Signature Verification
  -------------------------------------------------------------------
  describe "Signature Verification" $ do
    it "dev-mode returns the first user unconditionally" $ do
      let req    = defaultRequest
      let users  = [User "alice" (AccessKey "AK") (SecretKey "sk") []]
      result <- verifySigV4 True users req
      result `shouldBe` Right (head users)

    it "dev-mode returns Left when user list is empty" $ do
      let req   = defaultRequest
      let users = [] :: [User]
      result <- verifySigV4 True users req
      result `shouldBe` Left "No users configured"

    it "rejects request with missing Authorization header" $ do
      let req    = defaultRequest
      let users  = [User "test" (AccessKey "AKID") (SecretKey "secret") []]
      result <- verifySigV4 False users req
      result `shouldBe` Left "Missing Authorization header"

    it "rejects request with missing X-Amz-Date header" $ do
      let req = defaultRequest
            { requestHeaders =
                [ ("Authorization", "AWS4-HMAC-SHA256 Credential=AKID/20150830/us-east-1/s3/aws4_request, SignedHeaders=host, Signature=abc")
                ]
            }
      let users = [User "test" (AccessKey "AKID") (SecretKey "secret") []]
      result <- verifySigV4 False users req
      result `shouldBe` Left "Missing X-Amz-Date header"

    it "rejects request with unknown access key" $ do
      let req = defaultRequest
            { requestHeaders =
                [ ("Authorization", "AWS4-HMAC-SHA256 Credential=UNKNOWN/20150830/us-east-1/s3/aws4_request, SignedHeaders=host, Signature=abc")
                , ("X-Amz-Date", "20150830T123600Z")
                ]
            }
      let users = [User "test" (AccessKey "AKID") (SecretKey "secret") []]
      result <- verifySigV4 False users req
      result `shouldBe` Left "Invalid access key"

    it "rejects request with invalid signature" $ do
      let secret = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , requestHeaders =
                [ ("Authorization", "AWS4-HMAC-SHA256 Credential=AKID/20150830/us-east-1/s3/aws4_request, SignedHeaders=host, Signature=deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
                , ("X-Amz-Date", "20150830T123600Z")
                , ("Host", "example.com")
                ]
            }
      let users = [User "test" (AccessKey "AKID") secret []]
      result <- verifySigV4 False users req
      result `shouldBe` Left "Signature mismatch"

    it "rejects request when X-Amz-Date does not match credential scope date" $ do
      -- Credential date = "20210101", X-Amz-Date = "20150830T123600Z"
      let secret = SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      let req = defaultRequest
            { requestMethod  = "GET"
            , rawPathInfo    = "/"
            , requestHeaders =
                [ ("Authorization", "AWS4-HMAC-SHA256 Credential=AKID/20210101/us-east-1/s3/aws4_request, SignedHeaders=host, Signature=abc")
                , ("X-Amz-Date", "20150830T123600Z")
                , ("Host", "example.com")
                ]
            }
      let users = [User "test" (AccessKey "AKID") secret []]
      result <- verifySigV4 False users req
      result `shouldBe` Left "X-Amz-Date does not match credential scope date"
