{-# LANGUAGE OverloadedStrings #-}
module Main where
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString as B
import qualified Data.ByteArray as BA

main :: IO ()
main = do
  let secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" :: B.ByteString
  let date   = "20150830" :: B.ByteString
  let region = "us-east-1" :: B.ByteString
  let service = "iam" :: B.ByteString

  let kDate = hmacGetDigest $ (hmac ("AWS4" <> secret) date :: HMAC SHA256)
  let kRegion = hmacGetDigest $ (hmac kDate region :: HMAC SHA256)
  let kService = hmacGetDigest $ (hmac kRegion service :: HMAC SHA256)
  let kSigning = hmacGetDigest $ (hmac kService ("aws4_request" :: B.ByteString) :: HMAC SHA256)

  putStrLn $ show kSigning
