module Main where

import Test.Hspec
import qualified S3OSS.XMLSpec
import qualified S3OSS.Auth.PolicySpec
import qualified S3OSS.Object.StorageSpec
import qualified S3OSS.Auth.SigV4Spec
import qualified S3OSS.Bucket.HandlerSpec
import qualified S3OSS.Multipart.ManagerSpec

main :: IO ()
main = hspec $ do
  S3OSS.XMLSpec.spec
  S3OSS.Auth.PolicySpec.spec
  S3OSS.Object.StorageSpec.spec
  S3OSS.Auth.SigV4Spec.spec
  S3OSS.Bucket.HandlerSpec.spec
  S3OSS.Multipart.ManagerSpec.spec
