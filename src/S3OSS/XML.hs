{-# LANGUAGE OverloadedStrings #-}

-- | S3 XML serialization and deserialization.
module S3OSS.XML where

import RIO
import S3OSS.Types
import Data.Time (UTCTime)
import qualified RIO.Text as T
import qualified Data.ByteString.Lazy as BL
import Text.XML (Document(..), Prologue(..), Element(..), Node(..), Name(..))
import qualified Text.XML as X

-- | Render XML document as lazy ByteString.
renderLBS :: Document -> BL.ByteString
renderLBS doc = X.renderLBS X.def doc

-- | Create an XML element node.
elt :: Text -> [Node] -> Node
elt name children = NodeElement $ Element (Name name Nothing Nothing) mempty children

-- | Create a simple content node.
content :: Text -> Node
content = NodeContent

-- | Render ListBucketsResult.
renderListBucketsResult :: OwnerInfo -> [BucketInfo] -> Document
renderListBucketsResult owner buckets =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "ListBucketsResult" Nothing Nothing) mempty
      [ elt "Owner"
        [ elt "ID" [content $ ownerId owner]
        , elt "DisplayName" [content $ ownerDisplayName owner]
        ]
      , elt "Buckets" (map renderBucket buckets)
      ]

renderBucket :: BucketInfo -> Node
renderBucket bi =
  elt "Bucket"
    [ elt "Name" [content $ unBucketName $ biName bi]
    , elt "CreationDate" [content $ tshow $ biCreatedAt bi]
    ]

-- | Render Error response.
renderError :: Text -> Text -> Document
renderError code message =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "Error" Nothing Nothing) mempty
      [ elt "Code" [content code]
      , elt "Message" [content message]
      ]

-- | Render InitiateMultipartUploadResult.
renderInitiateMultipartUpload :: BucketName -> ObjectKey -> UploadId -> Document
renderInitiateMultipartUpload bucket key uploadId =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "InitiateMultipartUploadResult" Nothing Nothing) mempty
      [ elt "Bucket" [content $ unBucketName bucket]
      , elt "Key" [content $ unObjectKey key]
      , elt "UploadId" [content $ unUploadId uploadId]
      ]

-- | Render CompleteMultipartUploadResult.
renderCompleteMultipartUpload :: BucketName -> ObjectKey -> ETag -> Document
renderCompleteMultipartUpload bucket key etag =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "CompleteMultipartUploadResult" Nothing Nothing) mempty
      [ elt "Location" [content $ "/" <> unBucketName bucket <> "/" <> unObjectKey key]
      , elt "Bucket" [content $ unBucketName bucket]
      , elt "Key" [content $ unObjectKey key]
      , elt "ETag" [content $ unETag etag]
      ]

-- | Render CopyObjectResult.
renderCopyObjectResult :: ETag -> UTCTime -> Document
renderCopyObjectResult etag lastModified =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "CopyObjectResult" Nothing Nothing) mempty
      [ elt "ETag" [content $ unETag etag]
      , elt "LastModified" [content $ tshow lastModified]
      ]

-- | Render ListObjects response.
renderListObjects :: BucketName -> Maybe Text -> Maybe Text -> Int -> Bool -> [ObjectInfo] -> Document
renderListObjects bucket prefix delimiter maxKeys isTruncated objects =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (Name "ListBucketResult" Nothing Nothing) mempty
      ( [ elt "Name" [content $ unBucketName bucket]
        , elt "IsTruncated" [content $ if isTruncated then "true" else "false"]
        , elt "MaxKeys" [content $ tshow maxKeys]
        ]
      ++ maybe [] (\p -> [elt "Prefix" [content p]]) prefix
      ++ maybe [] (\d -> [elt "Delimiter" [content d]]) delimiter
      ++ [if null objects
            then elt "Contents" []
            else NodeElement $ Element (Name "Contents" Nothing Nothing) mempty (map renderObjectInfo objects)
         ]
      )

renderObjectInfo :: ObjectInfo -> Node
renderObjectInfo oi =
  elt "Contents"
    [ elt "Key" [content $ unObjectKey $ oiKey oi]
    , elt "Size" [content $ tshow $ oiSize oi]
    , elt "ETag" [content $ unETag $ oiETag oi]
    , elt "LastModified" [content $ tshow $ oiUpdatedAt oi]
    ]

-- | Parse CompleteMultipartUpload request body.
parseCompleteMultipartUpload :: BL.ByteString -> Either Text [(PartNumber, ETag)]
parseCompleteMultipartUpload body = do
  doc <- first tshow $ X.parseLBS X.def body
  let root = X.documentRoot doc
      parts = childElements root
  traverse parsePart parts
  where
    parsePart el = do
      let kids = childElements el
          pnNode = filter (\n -> X.nameLocalName (X.elementName n) == "PartNumber") kids
          etNode = filter (\n -> X.nameLocalName (X.elementName n) == "ETag") kids
      pnText <- case pnNode of
                  (e:_) -> Right $ textContent e
                  _     -> Left "Missing PartNumber"
      etagText <- case etNode of
                    (e:_) -> Right $ textContent e
                    _     -> Left "Missing ETag"
      pn <- case readMaybe (T.unpack pnText) of
              Just n  -> Right (PartNumber n)
              Nothing -> Left "Invalid PartNumber"
      pure (pn, ETag etagText)

-- XML helpers

-- | Get child Element nodes from an Element.
childElements :: Element -> [Element]
childElements el = [e | NodeElement e <- X.elementNodes el]

-- | Get all text content from an Element's child nodes.
textContent :: Element -> Text
textContent el = T.concat [t | NodeContent t <- X.elementNodes el]
