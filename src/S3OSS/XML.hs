{-# LANGUAGE OverloadedStrings #-}

-- | S3 XML serialization and deserialization.
module S3OSS.XML
  ( renderLBS
  , elt
  , content
  , renderListBucketsResult
  , renderBucket
  , renderError
  , renderInitiateMultipartUpload
  , renderCompleteMultipartUpload
  , renderCopyObjectResult
  , renderListObjects
  , renderCommonPrefix
  , renderObjectInfo
  , parseCompleteMultipartUpload
  , parseError
  ) where

import RIO
import S3OSS.Types
import Data.Time (UTCTime)
import qualified RIO.Text as T
import qualified Data.ByteString.Lazy as BL
import Text.XML (Document(..), Prologue(..), Element(..), Node(..), Name(..))
import qualified Text.XML as X

-- | S3 namespace helper.
s3Name :: Text -> Name
s3Name nm = Name nm (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing

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
    root = Element (s3Name "ListBucketsResult") mempty
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
    root = Element (s3Name "Error") mempty
      [ elt "Code" [content code]
      , elt "Message" [content message]
      ]

-- | Render InitiateMultipartUploadResult.
renderInitiateMultipartUpload :: BucketName -> ObjectKey -> UploadId -> Document
renderInitiateMultipartUpload bucket key uploadId =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (s3Name "InitiateMultipartUploadResult") mempty
      [ elt "Bucket" [content $ unBucketName bucket]
      , elt "Key" [content $ unObjectKey key]
      , elt "UploadId" [content $ unUploadId uploadId]
      ]

-- | Render CompleteMultipartUploadResult.
renderCompleteMultipartUpload :: BucketName -> ObjectKey -> ETag -> Document
renderCompleteMultipartUpload bucket key etag =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (s3Name "CompleteMultipartUploadResult") mempty
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
    root = Element (s3Name "CopyObjectResult") mempty
      [ elt "ETag" [content $ unETag etag]
      , elt "LastModified" [content $ tshow lastModified]
      ]

-- | Render ListObjects response. Handles both ListObjectsV1 and ListObjectsV2.
-- When isV2 is True, uses ContinuationToken/NextContinuationToken labels and adds KeyCount.
renderListObjects :: BucketName -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Bool -> [ObjectInfo] -> [Text] -> Maybe Text -> Bool -> Document
renderListObjects bucket prefix delimiter marker maxKeys isTruncated objects commonPrefixes nextToken isV2 =
  Document (Prologue [] Nothing []) root []
  where
    root = Element (s3Name "ListBucketResult") mempty
      ( [ elt "Name" [content $ unBucketName bucket]
        , elt "Prefix" [content $ fromMaybe "" prefix]
        , elt markerLabel [content $ fromMaybe "" marker]
        , elt "Delimiter" [content $ fromMaybe "" delimiter]
        , elt "IsTruncated" [content $ if isTruncated then "true" else "false"]
        , elt "MaxKeys" [content $ tshow maxKeys]
        ]
      ++ [elt "KeyCount" [content $ tshow $ length objects] | isV2]
      ++ map renderCommonPrefix commonPrefixes
      ++ map renderObjectInfo objects
      ++ nextMarkerElt
      )

    markerLabel = if isV2 then "ContinuationToken" else "Marker"

    nextMarkerElt = case nextToken of
      Just nm -> [elt (if isV2 then "NextContinuationToken" else "NextMarker") [content nm]]
      Nothing -> []

renderCommonPrefix :: Text -> Node
renderCommonPrefix p =
  elt "CommonPrefixes"
    [ elt "Prefix" [content p] ]

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
      rootName = X.nameLocalName (X.elementName root)
  unless (rootName == "CompleteMultipartUpload") $
    Left "Expected CompleteMultipartUpload element"
  let parts = childElements root
  traverse parsePart parts
  where
    parsePart el = do
      let kids = childElements el
          pnNode = filter (\n -> X.nameLocalName (X.elementName n) == "PartNumber") kids
          etNode = filter (\n -> X.nameLocalName (X.elementName n) == "ETag") kids
      pnText <- case pnNode of
                  [e]      -> Right $ textContent e
                  (e:_)    -> Right $ textContent e  -- duplicate(s) found, using first
                  []       -> Left "Missing PartNumber"
      etagText <- case etNode of
                    [e]      -> Right $ textContent e
                    (e:_)    -> Right $ textContent e  -- duplicate(s) found, using first
                    []       -> Left "Missing ETag"
      pn <- case readMaybe (T.unpack pnText) of
              Just n  -> Right (PartNumber n)
              Nothing -> Left "Invalid PartNumber"
      pure (pn, ETag etagText)

-- | Parse Error response XML. Returns (Code, Message).
parseError :: BL.ByteString -> Either Text (Text, Text)
parseError body = do
  doc <- first tshow $ X.parseLBS X.def body
  let root = X.documentRoot doc
      children = childElements root
  codeText <- case filter (\e -> X.nameLocalName (X.elementName e) == "Code") children of
                [e] -> Right $ textContent e
                (_:_) -> Left "Multiple Code elements"
                [] -> Left "Missing Code element"
  msgText <- case filter (\e -> X.nameLocalName (X.elementName e) == "Message") children of
               [e] -> Right $ textContent e
               (_:_) -> Left "Multiple Message elements"
               [] -> Left "Missing Message element"
  pure (codeText, msgText)

-- XML helpers

-- | Get child Element nodes from an Element.
childElements :: Element -> [Element]
childElements el = [e | NodeElement e <- X.elementNodes el]

-- | Get all text content from an Element's child nodes.
textContent :: Element -> Text
textContent el = T.strip $ T.concat [t | NodeContent t <- X.elementNodes el]
