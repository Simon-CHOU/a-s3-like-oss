{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module S3OSS.TypesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import S3OSS.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad (replicateM)
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genSha256Hex :: Gen Sha256Hex
genSha256Hex = Sha256Hex . T.pack <$> replicateM 64 (elements "0123456789abcdef")

genETag :: Gen ETag
genETag = do
  hex <- replicateM 32 (elements "0123456789abcdef")
  pure $ ETag ("\"" <> T.pack hex <> "\"")

genBucketName :: Gen BucketName
genBucketName = do
  c <- elements ['a'..'z']
  midLen <- choose (1, 61)
  mid <- replicateM midLen (elements "abcdefghijklmnopqrstuvwxyz0123456789-.")
  let raw = T.pack (c : mid ++ [c])
  if ".." `T.isInfixOf` raw
    then genBucketName
    else pure $ BucketName raw

genObjectKey :: Gen ObjectKey
genObjectKey = do
  len <- choose (1, 200)
  xs <- replicateM len (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~!$&'()*+,;=:@/ ")
  pure $ ObjectKey (T.pack xs)

genAccessKey :: Gen AccessKey
genAccessKey = AccessKey . T.pack <$> replicateM 20 (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

genSecretKey :: Gen SecretKey
genSecretKey = do
  len <- choose (0, 64)
  bs <- BS.pack <$> replicateM len (arbitrary :: Gen Word8)
  pure $ SecretKey bs

genUploadId :: Gen UploadId
genUploadId = UploadId . T.pack <$> replicateM 32 (elements "0123456789abcdef")

genPartNumber :: Gen PartNumber
genPartNumber = PartNumber <$> choose (1, 10000)

genResourceARN :: Gen ResourceARN
genResourceARN = ResourceARN . T.pack <$> replicateM 10 (elements "abcdefghijklmnopqrstuvwxyz0123456789:")

genAction :: Gen Action
genAction = elements
  [ S3GetObject, S3PutObject, S3DeleteObject, S3HeadObject
  , S3CopyObject, S3ListObjects, S3CreateBucket, S3DeleteBucket
  , S3ListBuckets, S3HeadBucket, S3CreateMultipartUpload
  , S3UploadPart, S3CompleteMultipartUpload, S3AbortMultipartUpload
  , S3AllActions
  ]

genEffect :: Gen Effect
genEffect = elements [Allow, Deny]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Sha256Hex where
  arbitrary = genSha256Hex
  shrink (Sha256Hex t) =
    [ Sha256Hex (T.take n t) | n <- [0, 8, 16, 32, 64], T.length t > n ]

instance Arbitrary ETag where
  arbitrary = genETag
  shrink (ETag t) =
    [ ETag (T.take n t) | n <- [0, 8, 16, 32], T.length t > n ]

instance Arbitrary BucketName where
  arbitrary = genBucketName

instance Arbitrary ObjectKey where
  arbitrary = genObjectKey

instance Arbitrary AccessKey where
  arbitrary = genAccessKey

instance Arbitrary SecretKey where
  arbitrary = genSecretKey
  shrink (SecretKey bs) = [SecretKey (BS.take n bs) | n <- [0, 8, 16, 32], BS.length bs > n]

instance Arbitrary UploadId where
  arbitrary = genUploadId

instance Arbitrary PartNumber where
  arbitrary = genPartNumber
  shrink (PartNumber n) =
    [ PartNumber n' | n' <- shrink n, n' >= 1, n' <= 10000 ]

instance Arbitrary ResourceARN where
  arbitrary = genResourceARN
  shrink (ResourceARN t) =
    [ ResourceARN (T.take n t) | n <- [0, 4, 8], T.length t > n ]

instance Arbitrary Action where
  arbitrary = genAction

instance Arbitrary Effect where
  arbitrary = genEffect

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "Sha256Hex" $ do
    prop "roundtrip: unSha256Hex . Sha256Hex preserves value" $ \(sha :: Sha256Hex) ->
      sha `shouldBe` Sha256Hex (unSha256Hex sha)

    prop "is exactly 64 hex characters" $ \(sha :: Sha256Hex) ->
      let t = unSha256Hex sha
      in (T.length t `shouldBe` 64)

    prop "contains only lowercase hex digits" $ \(sha :: Sha256Hex) ->
      let t = unSha256Hex sha
          isLowerHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      in (T.all isLowerHex t `shouldBe` True)

  describe "ETag" $ do
    prop "roundtrip: unETag . ETag preserves value" $ \(et :: ETag) ->
      (et `shouldBe` ETag (unETag et))

    prop "is quoted string" $ \(et :: ETag) ->
      let t = unETag et
      in ((T.length t >= 2) `shouldBe` True)

    prop "starts with a quote" $ \(et :: ETag) ->
      ((T.head (unETag et) == '"') `shouldBe` True)

    prop "ends with a quote" $ \(et :: ETag) ->
      ((T.last (unETag et) == '"') `shouldBe` True)

  describe "BucketName" $ do
    prop "roundtrip: mkBucketName . unBucketName succeeds for valid name" $ \(bn :: BucketName) ->
      mkBucketName (unBucketName bn) `shouldBe` Right bn

    it "accepts minimal valid name (3 chars)" $
      mkBucketName "abc" `shouldBe` Right (BucketName "abc")

    it "accepts maximal valid name (63 chars)" $
      let s = T.replicate 63 "a"
      in (mkBucketName s `shouldBe` Right (BucketName s))

    it "accepts name with dots" $
      mkBucketName "my.bucket.example" `shouldBe` Right (BucketName "my.bucket.example")

    it "accepts name with hyphens" $
      mkBucketName "my-bucket-name" `shouldBe` Right (BucketName "my-bucket-name")

    it "accepts name with numbers" $
      mkBucketName "bucket123" `shouldBe` Right (BucketName "bucket123")

    it "rejects empty name" $
      mkBucketName "" `shouldBe` Left "Bucket name must be between 3 and 63 characters"

    it "rejects name that is too short (2 chars)" $
      mkBucketName "ab" `shouldBe` Left "Bucket name must be between 3 and 63 characters"

    it "rejects name that is too long (64 chars)" $
      mkBucketName (T.replicate 64 "a") `shouldBe` Left "Bucket name must be between 3 and 63 characters"

    it "rejects uppercase letters" $
      mkBucketName "MyBucket" `shouldSatisfy` isLeft

    it "rejects underscores" $
      mkBucketName "my_bucket" `shouldSatisfy` isLeft

    it "rejects spaces" $
      mkBucketName "my bucket" `shouldSatisfy` isLeft

    it "rejects consecutive dots" $
      mkBucketName "my..bucket" `shouldSatisfy` isLeft

    it "rejects name starting with dot" $
      mkBucketName ".mybucket" `shouldSatisfy` isLeft

    it "rejects name ending with dot" $
      mkBucketName "mybucket." `shouldSatisfy` isLeft

    it "rejects name starting with hyphen" $
      mkBucketName "-mybucket" `shouldSatisfy` isLeft

    it "rejects name ending with hyphen" $
      mkBucketName "mybucket-" `shouldSatisfy` isLeft

    it "rejects name starting with a number" $
      mkBucketName "1bucket" `shouldSatisfy` isLeft

    it "rejects IP-style name (all numeric segments)" $
      mkBucketName "192.168.1.1" `shouldSatisfy` isLeft

    it "rejects name with special characters" $
      mkBucketName "bucket@name!" `shouldSatisfy` isLeft

  describe "ObjectKey" $ do
    prop "roundtrip: mkObjectKey . unObjectKey succeeds for valid key" $ \(ok :: ObjectKey) ->
      mkObjectKey (unObjectKey ok) `shouldBe` Right ok

    it "accepts single character key" $
      mkObjectKey "a" `shouldBe` Right (ObjectKey "a")

    it "accepts key with special characters" $
      mkObjectKey "a/b/c/file name (1).txt" `shouldBe` Right (ObjectKey "a/b/c/file name (1).txt")

    it "accepts key with Unicode characters" $
      mkObjectKey "fotoñ.txt" `shouldBe` Right (ObjectKey "fotoñ.txt")

    it "accepts long key (900 characters)" $
      let s = T.replicate 900 "a"
      in (mkObjectKey s `shouldBe` Right (ObjectKey s))

    it "rejects empty key" $
      mkObjectKey "" `shouldBe` Left "Object key must not be empty"

  describe "AccessKey" $ do
    prop "roundtrip: unAccessKey . AccessKey preserves value" $ \(ak :: AccessKey) ->
      ak `shouldBe` AccessKey (unAccessKey ak)

  describe "SecretKey" $ do
    prop "roundtrip: unSecretKey . SecretKey preserves value" $ \(sk :: SecretKey) ->
      sk `shouldBe` SecretKey (unSecretKey sk)

    it "does not show secret key contents" $
      show (SecretKey "mysecret123") `shouldBe` "SecretKey {unSecretKey = <secret>}"

  describe "UploadId" $ do
    prop "roundtrip: unUploadId . UploadId preserves value" $ \(uid :: UploadId) ->
      uid `shouldBe` UploadId (unUploadId uid)

  describe "PartNumber" $ do
    prop "roundtrip: mkPartNumber . unPartNumber succeeds for valid part number" $ \(pn :: PartNumber) ->
      mkPartNumber (unPartNumber pn) `shouldBe` Right pn

    it "accepts minimum part number (1)" $
      mkPartNumber 1 `shouldBe` Right (PartNumber 1)

    it "accepts maximum part number (10000)" $
      mkPartNumber 10000 `shouldBe` Right (PartNumber 10000)

    it "rejects part number 0" $
      mkPartNumber 0 `shouldBe` Left "Part number must be between 1 and 10000"

    it "rejects part number 10001" $
      mkPartNumber 10001 `shouldBe` Left "Part number must be between 1 and 10000"

    it "rejects negative part number" $
      mkPartNumber (-1) `shouldSatisfy` isLeft

  describe "ResourceARN" $ do
    prop "roundtrip: unResourceARN . ResourceARN preserves value" $ \(arn :: ResourceARN) ->
      arn `shouldBe` ResourceARN (unResourceARN arn)

  describe "Action" $ do
    prop "all 15 constructors are covered by exhaustive pattern match" $ \(a :: Action) ->
      ( case a of
          S3GetObject             -> True
          S3PutObject             -> True
          S3DeleteObject          -> True
          S3HeadObject            -> True
          S3CopyObject            -> True
          S3ListObjects           -> True
          S3CreateBucket          -> True
          S3DeleteBucket          -> True
          S3ListBuckets           -> True
          S3HeadBucket            -> True
          S3CreateMultipartUpload -> True
          S3UploadPart            -> True
          S3CompleteMultipartUpload -> True
          S3AbortMultipartUpload  -> True
          S3AllActions            -> True
      ) `shouldBe` True

    it "has 15 distinct constructors" $ do
      length [minBound :: Action .. maxBound] `shouldBe` 15

    it "S3AllActions is the maximum" $
      S3AllActions `shouldBe` maxBound

    prop "show produces readable string" $ \(a :: Action) ->
      show a `shouldNotBe` ""

  describe "Effect" $ do
    it "has Allow and Deny" $ do
      Allow `shouldNotBe` Deny

    prop "show is readable" $ \(e :: Effect) ->
      show e `shouldNotBe` ""
