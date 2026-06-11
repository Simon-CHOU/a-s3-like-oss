module Main where

import Test.Hspec
import qualified S3OSS.XMLSpec
import qualified S3OSS.Auth.PolicySpec
import qualified S3OSS.Object.StorageSpec

main :: IO ()
main = hspec $ do
  S3OSS.XMLSpec.spec
  S3OSS.Auth.PolicySpec.spec
  S3OSS.Object.StorageSpec.spec
