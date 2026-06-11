# S3-OSS Project Maturity Report

**Date:** 2026-06-12
**Assessment:** Pre-alpha

---

## 1. Overall Maturity Level: Pre-alpha

The project compiles and exposes a working HTTP server, but its core interoperability contracts with the S3 API are broken in ways that make it unusable with standard S3 clients.

### What compiles and runs
- SQLite metadata store (bucket CRUD, object metadata, multipart state machine) is functional
- Content-addressable filesystem storage with conduit-based streaming works correctly
- Bucket and object handler routing via raw WAI dispatches properly
- Policy evaluation (Allow/Deny) is logically sound
- Config loader and CLI parser work

### What is broken
- **SigV4 authentication**: `buildCanonicalRequest` includes ALL request headers rather than only those listed in signed headers, does not sort them — every AWS SDK client will fail
- **Presigned URLs**: `sigHex` renders via `show` on `ByteString` (produces Word8 list, not hex); signing scheme does not follow SigV4 spec
- **XML ListObjects**: Redundant parent `<Contents>` wrapping; self-closing `<Contents/>` when empty

---

## 2. Module Breakdown (16 modules)

| Module | Status | % | Tests |
|--------|--------|---|-------|
| S3OSS.Prelude | ✅ Complete | 100% | none |
| S3OSS.Types | ⚠️ Partial | 65% | none |
| S3OSS.Config | ✅ Complete | 92% | none |
| S3OSS.Store | ⚠️ Partial | 88% | none |
| S3OSS.XML | ⚠️ Partial | 75% | minimal |
| S3OSS.Auth.SigV4 | ⚠️ Partial | 50% | minimal |
| S3OSS.Auth.Policy | ✅ Complete | 100% | good |
| S3OSS.Object.Storage | ⚠️ Partial | 82% | minimal |
| S3OSS.Bucket.Handler | ✅ Complete | 95% | stub |
| S3OSS.Object.Handler | ⚠️ Partial | 65% | none |
| S3OSS.Multipart.Manager | ⚠️ Partial | 75% | stub |
| S3OSS.Multipart.Handler | ✅ Complete | 95% | none |
| S3OSS.List.Handler | ⚠️ Partial | 60% | none |
| S3OSS.Presigned | ❌ Broken | 25% | none |
| S3OSS.Server | ✅ Complete | 90% | none |
| Main (CLI) | ✅ Complete | 85% | none |

**Summary**: 7 complete, 8 partial, 1 broken, 0 stubs. Tests: 5/16 have tests (1 good quality).

---

## 3. Critical Gaps

### Blocks actual usage

| Severity | Issue | Module |
|----------|-------|--------|
| CRITICAL | SigV4 canonical request includes all headers unsorted | Auth.SigV4 |
| CRITICAL | Presigned URL signature is unverifiable | Presigned |
| HIGH | XML ListObjects wraps objects in redundant `<Contents>` | XML |
| HIGH | `metaPairs` silently ignored in putObjectMeta | Store |
| HIGH | PUT object in Server bypasses handler module | Server |
| HIGH | ETag validation absent in completeUpload | Multipart.Manager |
| MEDIUM | SHA256/MD5 type confusion in uploadPart | Multipart.Manager |
| MEDIUM | Concurrent upload temp file collision | Object.Storage |
| MEDIUM | Content-Type cannot be set on upload | Object.Handler |

### Missing from design spec
- `Md5Hex` newtype absent
- `PartNumber` missing FromJSON/ToJSON
- `ListObjectsV2` not implemented
- Range header support for GetObject not implemented
- Composite ETag format for multipart not implemented

---

## 4. Architectural Strengths

1. **Clean layered separation** — Auth, Object, Bucket, Multipart, List as subdirectories
2. **Conduit-based streaming throughout** — uniform `ConduitT () ByteString IO ()` interface
3. **Content-addressable storage** — SHA-256 + atomic rename, automatic dedup
4. **Deny-overrides-Allow policy evaluation** — clean 8-line implementation
5. **Consistent error response pattern** — S3 XML errors across all handlers
6. **Functional config resolution** — `Either Text ResolvedConfig` rather than exceptions

---

## 5. Next Priorities

1. **P0**: Fix SigV4 canonical request construction (filter + sort signed headers)
2. **P0**: Fix Presigned URL signature generation (hex encoding + proper SigV4 scheme)
3. **P1**: Fix XML ListObjects response structure
4. **P1**: Wire all test modules and add handler integration tests
5. **P2**: Fix Server routing bypass for PutObject to delegate through handler
