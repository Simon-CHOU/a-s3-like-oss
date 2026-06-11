# s3-oss: A Secure S3-Like Local-First OSS Service in Haskell

**Date:** 2026-06-11
**Status:** Draft
**Scope:** Single Haskell library + executable providing an S3-compatible, secure, local-first object storage service.

---

## 1. Overview & Goals

Build a self-contained, single-binary object storage service written in Haskell that speaks the S3 REST API over HTTPS, authenticates requests via AWS SigV4, enforces IAM-like authorization policies, and stores objects on the local filesystem using content-addressable storage.

**Non-goals (explicitly excluded for this iteration):**
- Distributed / multi-node replication (this is "local-first")
- Server-side encryption (SSE-S3, SSE-KMS) — deferred, not excluded
- Object versioning — deferred
- S3 Event Notifications — deferred
- Object Lock / WORM — deferred

---

## 2. Feature Scope

### 2.1 Bucket Operations
- `CreateBucket` — create a new bucket
- `DeleteBucket` — delete an empty bucket
- `ListBuckets` — list all buckets for the authenticated user
- `HeadBucket` — check if a bucket exists

### 2.2 Object Operations
- `PutObject` — create/overwrite an object (streaming)
- `GetObject` — retrieve an object (streaming, supports Range)
- `DeleteObject` — delete an object
- `HeadObject` — retrieve object metadata without body
- `CopyObject` — server-side copy within/between buckets
- `ListObjects` / `ListObjectsV2` — list objects in a bucket (prefix, delimiter, pagination)

### 2.3 Multipart Uploads
- `CreateMultipartUpload` — initiate a multipart upload
- `UploadPart` — upload a part (1-10000 parts, 5MB-5GB each, except last)
- `CompleteMultipartUpload` — finalize and assemble all parts
- `AbortMultipartUpload` — cancel and clean up
- Background GC for abandoned uploads (7-day expiry)

### 2.4 Authentication & Authorization
- AWS Signature Version 4 (SigV4) request signing verification
- User management via config file (access key, bcrypt-hashed secret key)
- IAM-like policy engine (Allow/Deny, Action, Resource ARN matching)
- Deny-overrides-Allow evaluation
- 15-minute timestamp window for replay protection

### 2.5 Presigned URLs
- Generate presigned URLs for `GetObject` and `PutObject`
- Validate presigned URLs on request (signature + expiry check)
- Configurable default expiry (1 minute to 7 days)

### 2.6 Transport Security
- TLS 1.2+ via `warp-tls` (certificate and key from config)
- Optional: plain HTTP for localhost-only development mode

---

## 3. Architecture

### 3.1 Package Structure

```
s3-oss/
├── s3-oss.cabal
├── app/
│   └── Main.hs                    -- CLI arg parsing, server startup
├── src/
│   ├── S3OSS/
│   │   ├── Server.hs              -- Warp + TLS, middleware stack
│   │   ├── API.hs                 -- Servant type-level API definition (single source of truth)
│   │   ├── Auth/
│   │   │   ├── SigV4.hs           -- AWS SigV4 signature verification
│   │   │   └── Policy.hs          -- IAM-like policy evaluation engine
│   │   ├── Bucket/
│   │   │   ├── Handler.hs         -- Bucket HTTP handlers
│   │   │   └── Model.hs           -- Bucket data types
│   │   ├── Object/
│   │   │   ├── Handler.hs         -- Object HTTP handlers
│   │   │   ├── Model.hs           -- Object metadata types
│   │   │   └── Storage.hs         -- Content-addressable filesystem storage engine
│   │   ├── Multipart/
│   │   │   ├── Handler.hs         -- Multipart upload HTTP handlers
│   │   │   └── Manager.hs         -- Multipart state machine + GC
│   │   ├── List/
│   │   │   └── Handler.hs         -- ListObjects handlers
│   │   ├── Presigned.hs           -- Presigned URL generation + validation
│   │   ├── XML.hs                 -- S3 XML serialization/deserialization
│   │   └── Store.hs              -- SQLite metadata persistence layer
│   └── Prelude.hs                -- Custom prelude (rio or relude based)
└── test/
    ├── Spec.hs
    └── S3OSS/
        ├── Auth/
        │   ├── SigV4Spec.hs
        │   └── PolicySpec.hs
        ├── Object/
        │   └── StorageSpec.hs
        ├── Bucket/
        │   └── HandlerSpec.hs
        └── Multipart/
            └── ManagerSpec.hs
```

### 3.2 Request Data Flow

```
HTTP Request
  → Warp (TLS termination)
    → WAI Middleware (request logging, CORS)
      → Servant Router (API type dispatch)
        → Auth Middleware (SigV4 signature verification)
          → Policy Engine (IAM authorization decision)
            → Handler (business logic)
              → Store (SQLite metadata R/W)
              → Storage (filesystem object I/O via conduit)
            → S3 XML Response (blaze-markup + xml-conduit)
```

### 3.3 Object Storage Layout (on disk)

```
<data-dir>/
├── meta.sqlite                    -- All metadata (buckets, objects, multipart state, users)
└── objects/
    └── <sha256[0:2]>/             -- First 2 hex chars as shard directory
        └── <sha256>               -- Full hex-encoded SHA-256 as filename
```

**Properties:**
- **Content-addressable:** same content → same hash → same file (automatic dedup)
- **Integrity:** hash computed incrementally during write; can be verified on read
- **Atomic writes:** write to temp file, then `rename(2)` to final path
- **Immutable:** once written under hash, file content never changes
- **Deletion:** ref-count via SQLite; last ref removed → unlink file

### 3.4 Multipart Upload Assembly

Parts are stored individually:
```
<data-dir>/multipart/<upload-id>/part-<part-number>
```

On `CompleteMultipartUpload`:
1. Validate all listed parts exist and have correct ETags
2. Open part files sequentially via conduit
3. Stream concatenation into a single object (no full buffering)
4. Compute composite ETag: `<md5-of-concatenated-md5s>-<part-count>`
5. Atomically write final object to content-addressable location
6. Update SQLite object metadata
7. Delete part files

Background GC: a periodic thread (every 30 min) scans for uploads with `created_at > 7 days` and aborts them, cleaning up parts.

---

## 4. Data Models

### 4.1 User & Credentials

```haskell
data User = User
  { userName      :: Text
  , accessKey     :: AccessKey       -- "AKID..." (20 alphanumeric chars)
  , secretKeyHash :: ByteString      -- bcrypt/scrypt hash, never stored plaintext
  , policies      :: [Policy]
  }
```

Users loaded from config file at startup. Secret key only available at config load time; hash verified on each request.

### 4.2 Policy Model

```haskell
data Policy = Policy
  { effect    :: Effect              -- Allow | Deny
  , actions   :: [Action]            -- "s3:GetObject", "s3:PutObject", "s3:*", "*"
  , resources :: [ResourceARN]       -- "arn:aws:s3:::my-bucket", "arn:aws:s3:::my-bucket/*"
  }

data Action = GetObject | PutObject | DeleteObject | HeadObject | CopyObject
            | ListObjects | CreateBucket | DeleteBucket | ListBuckets | HeadBucket
            | CreateMultipartUpload | UploadPart | CompleteMultipartUpload | AbortMultipartUpload
            | AllActions
            deriving (Eq, Ord)

evaluate :: [Policy] -> Action -> ResourceARN -> Bool
-- Deny takes priority. If any Deny matches, return False.
-- If any Allow matches (and no Deny matches), return True.
-- If no policy matches, return False (default deny).
```

### 4.3 Object Metadata (in SQLite)

```sql
CREATE TABLE objects (
  id          INTEGER PRIMARY KEY,
  bucket_id   INTEGER NOT NULL REFERENCES buckets(id),
  key         TEXT NOT NULL,
  sha256      TEXT NOT NULL,           -- hex-encoded SHA-256
  size        INTEGER NOT NULL,        -- bytes
  content_type TEXT,
  etag        TEXT NOT NULL,
  metadata    TEXT,                     -- JSON blob for x-amz-meta-* key-value pairs
  ref_count   INTEGER NOT NULL DEFAULT 1,
  created_at  TEXT NOT NULL,            -- ISO-8601
  updated_at  TEXT NOT NULL,
  UNIQUE(bucket_id, key)
);

CREATE TABLE multipart_uploads (
  upload_id   TEXT PRIMARY KEY,
  bucket_id   INTEGER NOT NULL REFERENCES buckets(id),
  key         TEXT NOT NULL,
  state       TEXT NOT NULL,            -- 'initiated' | 'completed' | 'aborted'
  created_at  TEXT NOT NULL,
  expires_at  TEXT NOT NULL             -- created_at + 7 days
);

CREATE TABLE multipart_parts (
  upload_id   TEXT NOT NULL REFERENCES multipart_uploads(upload_id),
  part_number INTEGER NOT NULL,
  sha256      TEXT NOT NULL,
  size        INTEGER NOT NULL,
  etag        TEXT NOT NULL,
  PRIMARY KEY (upload_id, part_number)
);
```

---

## 5. Dependencies

| Category | Package | Purpose |
|----------|---------|---------|
| HTTP server | `warp`, `wai`, `wai-extra` | High-performance HTTP, middleware |
| API routing | `servant`, `servant-server` | Type-safe REST API |
| TLS | `warp-tls`, `tls` | Transport security |
| Cryptography | `crypton` | HMAC-SHA256, SHA-256, MD5, key derivation |
| Database | `sqlite-simple`, `direct-sqlite` | Metadata persistence |
| Streaming | `conduit`, `conduit-extra` | Memory-safe object I/O |
| XML | `xml-conduit`, `blaze-markup` | S3 XML request/response |
| Hashing | `bcrypt` or `scrypt` | Secret key hashing |
| Config | `yaml`, `optparse-applicative` | CLI args + config file |
| Time | `time` | Timestamps, expiry, clock skew |
| Logging | `fast-logger` | Structured request logging |
| Testing | `hspec`, `QuickCheck`, `temporary`, `hspec-wai` | Unit + integration tests |

---

## 6. Testing Strategy

### 6.1 Unit Tests
- **SigV4**: Known-answer tests against RFC example vectors
- **Policy evaluation**: QuickCheck properties — deny always wins, allow-only-when-no-deny-match, default-deny
- **XML serialize/parse**: Round-trip property tests for all S3 XML types
- **Storage engine**: Write-then-read integrity; corruption detection; atomic write semantics

### 6.2 Integration Tests
- Full HTTP handler tests via `hspec-wai` + temporary directory for data
- Multipart upload lifecycle (create → upload parts → complete → verify assembled object)
- Presigned URL generation and validation round-trip
- Bucket lifecycle (create → put object → list → delete object → delete bucket)

### 6.3 Compatibility Smoke Tests
- Use AWS CLI with `--endpoint-url http://localhost:9443` to verify real S3 client compatibility
- Test with `s3cmd`, `rclone`, or `minio-client (mc)` as additional clients

---

## 7. Configuration & Deployment

### 7.1 Config file (`~/.s3-oss/config.yaml`)

```yaml
server:
  host: "127.0.0.1"
  port: 9443
  tls:
    cert: "/etc/s3-oss/cert.pem"
    key: "/etc/s3-oss/key.pem"
  # development_mode: true  # disables TLS for localhost-only

storage:
  data_dir: "/var/lib/s3-oss"

users:
  - name: "admin"
    access_key: "AKID0000000000000000"
    policies:
      - effect: "Allow"
        actions: ["*"]
        resources: ["*"]
```

### 7.2 CLI

```
s3-oss --config /path/to/config.yaml
s3-oss --port 9443 --data-dir /data/s3 --tls-cert cert.pem --tls-key key.pem
```

CLI flags override config file values.

---

## 8. Key Design Decisions & Rationale

1. **Servant over raw WAI:** Servant's type-level API definition acts as living documentation and catches handler/serialization mismatches at compile time. The additional dependency weight is justified for a project with a well-defined, stable API surface.

2. **Content-addressable storage over bucket/key directory tree:** Deduplication and integrity verification come for free. Object keys can contain arbitrary characters including `/` and Unicode — no escaping needed for filesystem paths.

3. **SQLite over LMDB:** SQLite has richer query capabilities (filtering, sorting, pagination for ListObjects), stronger ACID guarantees, and broader Haskell ecosystem support. Performance is more than adequate for metadata operations.

4. **conduit over streaming or pipes:** The conduit ecosystem has the most mature support for HTTP request/response body streaming in WAI/Servant context. Conduit's `Source`/`Sink` model maps well to S3's put-object/get-object streaming patterns.

5. **SigV4 with configuration-managed users over OAuth/OIDC:** SigV4 is the native S3 auth protocol. Any S3 client (AWS CLI, boto3, minio-client) speaks it. Configuration-file user management avoids external IAM dependency while preserving the protocol.
