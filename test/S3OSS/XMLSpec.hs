{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module S3OSS.XMLSpec (spec) where

import Test.Hspec
import S3OSS.XML
import S3OSS.Types
import Data.Time (UTCTime(..), fromGregorian)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

spec :: Spec
spec = do
  describe "S3 XML serialization" $ do
    it "renders ListBucketsResult with one bucket" $ do
      let t = UTCTime (fromGregorian 2026 6 11) 0
      let buckets = [BucketInfo (BucketName "my-bucket") t]
          owner = OwnerInfo "test-user" "owner-id"
      let doc = renderListBucketsResult owner buckets
      let text = show (BL.toStrict $ renderLBS doc)
      text `shouldContain` "ListBucketsResult"
      text `shouldContain` "my-bucket"

    it "renders Error response" $ do
      let doc = renderError "NoSuchBucket" "The bucket does not exist"
      let text = show (BL.toStrict $ renderLBS doc)
      text `shouldContain` "Error"
      text `shouldContain` "NoSuchBucket"

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

    it "roundtrips InitiateMultipartUpload XML" $ do
      let doc = renderInitiateMultipartUpload (BucketName "bucket") (ObjectKey "key.txt") (UploadId "upload-123")
      let text = show (BL.toStrict $ renderLBS doc)
      text `shouldContain` "InitiateMultipartUploadResult"
      text `shouldContain` "bucket"
      text `shouldContain` "key.txt"
      text `shouldContain` "upload-123"
