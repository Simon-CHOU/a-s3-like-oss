{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.XMLSpec (spec) where

import Test.Hspec
import S3OSS.XML
import S3OSS.Types
import Data.Time (UTCTime(..), fromGregorian)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Either (isLeft)
import RIO (tshow)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "S3 XML serialization" $ do
    it "renders ListBucketsResult with one bucket" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let buckets = [BucketInfo (BucketName "my-bucket") t]
          owner = OwnerInfo "test-user" "owner-id"
      let doc = renderListBucketsResult owner buckets
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "ListBucketsResult"
      text `shouldContain` "my-bucket"
      text `shouldContain` "test-user"
      text `shouldContain` "owner-id"
      text `shouldContain` "2026-06-11"

    it "renders Error response with Code and Message" $ do
      let doc = renderError "NoSuchBucket" "The bucket does not exist"
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "Error"
      text `shouldContain` "NoSuchBucket"
      text `shouldContain` "The bucket does not exist"
      text `shouldContain` "Code"
      text `shouldContain` "Message"

    it "parses CompleteMultipartUpload request body" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<CompleteMultipartUpload>"
            , "<Part><PartNumber>1</PartNumber><ETag>\"abc123\"</ETag></Part>"
            , "<Part><PartNumber>2</PartNumber><ETag>\"def456\"</ETag></Part>"
            , "</CompleteMultipartUpload>"
            ]
      let result = parseCompleteMultipartUpload body
      result `shouldSatisfy` \case
        Right [(PartNumber 1, ETag "\"abc123\""), (PartNumber 2, ETag "\"def456\"")] -> True
        _ -> False

    it "parses CompleteMultipartUpload with single part" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<CompleteMultipartUpload>"
            , "<Part><PartNumber>1</PartNumber><ETag>\"etag\"</ETag></Part>"
            , "</CompleteMultipartUpload>"
            ]
      parseCompleteMultipartUpload body `shouldBe` Right [(PartNumber 1, ETag "\"etag\"")]

    it "roundtrips InitiateMultipartUpload XML" $ do
      let doc = renderInitiateMultipartUpload (BucketName "bucket") (ObjectKey "key.txt") (UploadId "upload-123")
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "InitiateMultipartUploadResult"
      text `shouldContain` "bucket"
      text `shouldContain` "key.txt"
      text `shouldContain` "upload-123"
      text `shouldContain` "Bucket"
      text `shouldContain` "Key"
      text `shouldContain` "UploadId"

    it "renders CompleteMultipartUploadResult" $ do
      let doc = renderCompleteMultipartUpload (BucketName "bucket") (ObjectKey "key.txt") (ETag "\"etag123\"")
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "CompleteMultipartUploadResult"
      text `shouldContain` "bucket/key.txt"
      text `shouldContain` "\"etag123\""

    it "renders CopyObjectResult" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let doc = renderCopyObjectResult (ETag "\"copied\"") t
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "CopyObjectResult"
      text `shouldContain` "\"copied\""
      text `shouldContain` "2026-06-11"

    it "renders ListBucketResult with objects and common prefixes" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let objects = [ObjectInfo (BucketName "b") (ObjectKey "a.txt") (Sha256Hex "abc") 123 Nothing [] (ETag "\"e\"") t t]
          prefixes = ["prefix/"]
      let doc = renderListObjects (BucketName "b") (Just "a") (Just "/") (Just "mark") 100 False objects prefixes Nothing False
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "ListBucketResult"
      text `shouldContain` "IsTruncated"
      text `shouldContain` "false"
      text `shouldContain` "100"
      text `shouldContain` "a.txt"
      text `shouldContain` "prefix/"
      text `shouldContain` "Marker"
      text `shouldContain` "mark"

  describe "Error XML roundtrip" $ do
    let s3ErrorMessages =
          [ ("AccessDenied", "Access Denied")
          , ("BucketAlreadyExists", "The requested bucket name is not available")
          , ("BucketNotEmpty", "The bucket you tried to delete is not empty")
          , ("InternalError", "We encountered an internal error")
          , ("InvalidArgument", "Invalid argument")
          , ("InvalidPart", "One or more of the specified parts could not be found")
          , ("MalformedXML", "The XML you provided was not well-formed")
          , ("MethodNotAllowed", "The specified method is not allowed against this resource")
          , ("NoSuchBucket", "The specified bucket does not exist")
          , ("NoSuchKey", "The specified key does not exist.")
          , ("NoSuchUpload", "The specified upload does not exist")
          , ("SignatureDoesNotMatch", "The request signature we calculated does not match")
          ]

    it "renders each S3 error code and message" $ do
      forM_ s3ErrorMessages $ \(code, msg) -> do
        let doc = renderError code msg
        let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
        text `shouldContain` T.unpack code
        text `shouldContain` T.unpack msg

    it "roundtrips Error XML for all S3 error codes" $ do
      forM_ s3ErrorMessages $ \(code, msg) -> do
        let doc = renderError code msg
        let bytes = renderLBS doc
        case parseError bytes of
          Left err -> expectationFailure $ "Failed to parse error XML for " ++ show code ++ ": " ++ show err
          Right (parsedCode, parsedMsg) -> do
            parsedCode `shouldBe` code
            parsedMsg `shouldBe` msg

    it "roundtrips Error XML with special characters" $ do
      let doc = renderError "NoSuchBucket" "Bucket <name> & \"quotes\""
      let bytes = renderLBS doc
      case parseError bytes of
        Left err -> expectationFailure $ "Failed to parse special chars: " ++ show err
        Right (code, msg) -> do
          code `shouldBe` "NoSuchBucket"
          msg `shouldBe` "Bucket <name> & \"quotes\""

    it "rejects Error XML with missing Code" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<Error>"
            , "<Message>test</Message>"
            , "</Error>"
            ]
      parseError body `shouldSatisfy` isLeft

    it "rejects Error XML with missing Message" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<Error>"
            , "<Code>NoSuchBucket</Code>"
            , "</Error>"
            ]
      parseError body `shouldSatisfy` isLeft

    it "rejects non-XML input for parseError" $ do
      let body = BL.fromStrict $ encodeUtf8 "not xml"
      parseError body `shouldSatisfy` isLeft

  describe "ListBucketResult with large number of objects" $ do
    it "renders 1000 objects correctly" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let objects = [ObjectInfo (BucketName "b") (ObjectKey $ "key-" <> tshow (i :: Int) <> ".txt") (Sha256Hex "abc") (fromIntegral i) Nothing [] (ETag "\"e\"") t t | i <- [1..1000]]
      let doc = renderListObjects (BucketName "test-bucket") Nothing Nothing Nothing 1000 True objects [] Nothing False
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "ListBucketResult"
      text `shouldContain` "IsTruncated"
      text `shouldContain` "true"
      text `shouldContain` "1000"
      text `shouldContain` "key-1.txt"
      text `shouldContain` "key-1000.txt"

    it "renders 2000 objects without performance issues" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let objects = [ObjectInfo (BucketName "b") (ObjectKey $ "obj-" <> tshow (i :: Int)) (Sha256Hex "abc") 0 Nothing [] (ETag "\"e\"") t t | i <- [1..2000]]
      let doc = renderListObjects (BucketName "b") Nothing Nothing Nothing 1000 True objects [] Nothing False
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "ListBucketResult"
      text `shouldContain` "obj-1"
      text `shouldContain` "obj-2000"

    it "renders 1000 objects with nextToken and isV2" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let objects = [ObjectInfo (BucketName "b") (ObjectKey $ "k-" <> tshow (i :: Int)) (Sha256Hex "abc") 0 Nothing [] (ETag "\"e\"") t t | i <- [1..1000]]
      let doc = renderListObjects (BucketName "b") Nothing Nothing Nothing 1000 True objects [] (Just "next-page") True
      let text = T.unpack (decodeUtf8 (BL.toStrict $ renderLBS doc))
      text `shouldContain` "KeyCount"
      text `shouldContain` "NextContinuationToken"
      text `shouldContain` "next-page"
      text `shouldContain` "k-1"
      text `shouldContain` "k-1000"

  describe "Malformed XML rejection" $ do
    it "rejects empty input" $ do
      parseCompleteMultipartUpload mempty `shouldSatisfy` isLeft

    it "rejects non-XML input" $ do
      let body = BL.fromStrict $ encodeUtf8 "this is not xml"
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects missing CompleteMultipartUpload element" $ do
      let body = BL.fromStrict $ encodeUtf8 "<WrongElement><Part><PartNumber>1</PartNumber><ETag>e</ETag></Part></WrongElement>"
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects missing PartNumber" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<CompleteMultipartUpload>"
            , "<Part><ETag>\"e\"</ETag></Part>"
            , "</CompleteMultipartUpload>"
            ]
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects missing ETag" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<CompleteMultipartUpload>"
            , "<Part><PartNumber>1</PartNumber></Part>"
            , "</CompleteMultipartUpload>"
            ]
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects non-numeric PartNumber" $ do
      let body = BL.fromStrict $ encodeUtf8 $ mconcat
            [ "<CompleteMultipartUpload>"
            , "<Part><PartNumber>abc</PartNumber><ETag>\"e\"</ETag></Part>"
            , "</CompleteMultipartUpload>"
            ]
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects truncated XML" $ do
      let body = BL.fromStrict $ encodeUtf8 "<CompleteMultipartUpload><Part><PartNumber>1</PartNumber"
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft

    it "rejects binary garbage" $ do
      let body = BL.fromStrict $ B.pack [0x00, 0x01, 0x02, 0xff]
      parseCompleteMultipartUpload body `shouldSatisfy` isLeft
