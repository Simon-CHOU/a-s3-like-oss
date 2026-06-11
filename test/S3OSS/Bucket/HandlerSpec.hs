module S3OSS.Bucket.HandlerSpec (spec) where

import Test.Hspec

-- Integration tests for bucket handlers will be added when hspec-wai setup is ready.
spec :: Spec
spec = do
  describe "Bucket Handlers" $ do
    it "placeholder test" $ do
      1 + 1 `shouldBe` (2 :: Int)
