# S3-OSS Project Maturity Report

**Date:** 2026-06-12
**Assessment:** Alpha (5.2/10) — upgraded from Pre-alpha after fix workflow
**Previous assessment:** Pre-alpha (see commit 256247e)

---

## 1. Overall Maturity Level: Alpha

The project has a sound architectural skeleton with all major layers present (Store, XML, Auth, Handlers, Server, Multipart). After the audit-driven fix workflow (553 insertions, 315 deletions across 15 files), the project **builds successfully** and cross-module integration is verified **coherent**. However, critical correctness and protocol-compliance defects in the runtime path prevent use with real S3 clients.

### What improved since Pre-alpha
- SigV4 canonical request now filters + sorts signed headers
- Presigned URL hex encoding fixed (was `show` → Word8 list)
- XML ListObjects Contents element structure corrected
- Store: safe Maybe/Either returns, delimiter filtering, proper ISO-8601
- Object.Storage: unique temp files, bracket-guarded cleanup
- Types: missing FromJSON/ToJSON instances added
- All handlers: explicit export lists

### What still blocks real S3 client usage
- **PUT authorization bypass**: Server routes PUT directly to `putObject`, skipping `handlePutObject` and all policy checks
- **CopyObject dead code**: `handleCopyObject` exists but has no route
- **ListObjectsV2**: not implemented despite being in the design spec
- **Pagination broken**: `isTruncated` always False; no `NextMarker`/`ContinuationToken`
- **XML namespace missing**: all responses lack `xmlns="http://s3.amazonaws.com/doc/2006-03-01/"`
- **No ETag on PUT responses**: clients hang waiting for it
- **Presigned URLs**: non-standard signing scheme; validation not wired into server

---

## 2. Module Scores (post-fix)

| Module | Correct | Complete | S3-Compat | Tests | Avg |
|--------|---------|----------|-----------|-------|-----|
| S3OSS.Prelude | 7/10 | 9/10 | 10/10 | 1/10 | 6.8 |
| S3OSS.Types | 7/10 | 7/10 | 7/10 | 5/10 | 6.5 |
| S3OSS.Config | 8/10 | 8/10 | 8/10 | 1/10 | 6.3 |
| S3OSS.Store | 7/10 | 6/10 | 4/10 | 1/10 | 4.5 |
| S3OSS.XML | 7/10 | 7/10 | 5/10 | 4/10 | 5.8 |
| S3OSS.Auth.SigV4 | 6/10 | 7/10 | 5/10 | 1/10 | 4.8 |
| **S3OSS.Auth.Policy** | **9/10** | **8/10** | **7/10** | **5/10** | **7.3** |
| S3OSS.Object.Storage | 7/10 | 6/10 | 7/10 | 4/10 | 6.0 |
| S3OSS.Bucket.Handler | 7/10 | 9/10 | 7/10 | 1/10 | 6.0 |
| S3OSS.Object.Handler | 6/10 | 4/10 | 5/10 | 1/10 | 4.0 |
| S3OSS.Multipart.Manager | 5/10 | 5/10 | 4/10 | 1/10 | 3.8 |
| S3OSS.Multipart.Handler | 8/10 | 8/10 | 7/10 | 1/10 | 6.0 |
| S3OSS.List.Handler | 5/10 | 5/10 | 4/10 | 1/10 | 3.8 |
| S3OSS.Presigned | 7/10 | 5/10 | 1/10 | 0/10 | 3.3 |
| S3OSS.Server | 6/10 | 5/10 | 3/10 | 1/10 | 3.8 |
| Main | 5/10 | 6/10 | 4/10 | 4/10 | 4.8 |

### Layer Averages

| Layer | Score | Verdict |
|-------|-------|---------|
| Foundation (Prelude, Types, Config) | 7.3 | Solid base, needs validation |
| Auth (SigV4, Policy, Presigned) | 5.1 | Policy is excellent; SigV4 adequate; Presigned broken |
| Data (Store) | 4.5 | Functional but buggy — metadata ignored, ref_count dead |
| Serialization (XML) | 5.8 | Missing namespace, dates, pagination markers |
| Storage (Object.Storage) | 6.0 | Works but leaks on failure, no read integrity check |
| Handlers (Bucket, Object, List, Multipart) | 5.0 | Dead code, broken pagination, no tests |
| Integration (Server, Main) | 4.3 | PUT bypass, missing routes, no middleware |

---

## 3. Production Readiness

### Could potentially be used today
- **S3OSS.Auth.Policy** — The ARN matching engine is well-tested and robust. Extractable as a standalone library.
- **S3OSS.Types** — Core domain types are well-designed.
- **S3OSS.Config** — Pure config resolution with clean Either-based error handling.

### Would fail under real load or real clients
Everything else. The problems are correctness and protocol compliance, not performance. Any real S3 client (aws-cli, boto3, rclone, minio-client) would fail on basic operations.

---

## 4. Critical Gaps (ranked)

| # | Severity | Issue | Module |
|---|----------|-------|--------|
| 1 | CRITICAL | PUT bypasses authorization entirely | Server |
| 2 | CRITICAL | No ETag on PUT responses | Object.Handler |
| 3 | CRITICAL | CopyObject has no route | Server |
| 4 | CRITICAL | ListObjectsV2 not implemented | List.Handler, Server |
| 5 | HIGH | Pagination broken (isTruncated always False) | List.Handler |
| 6 | HIGH | XML namespace missing on all responses | XML |
| 7 | HIGH | Presigned URL signing non-standard, never called | Presigned, Server |
| 8 | HIGH | metadata ignored in putObjectMeta | Store |
| 9 | MEDIUM | 3 orphaned test files not wired into Spec.hs | test/Spec.hs |
| 10 | MEDIUM | No replay protection window in SigV4 | Auth.SigV4 |
| 11 | MEDIUM | Prelude only used by 1 of 15 modules | Prelude |
| 12 | LOW | errorResponse copy-pasted across 5 modules | Handlers, Server |

---

## 5. Git History (7 commits, clean working tree)

```
8a4f101 fix: comprehensive audit-driven fixes across all modules
256247e feat: WAI application router, server startup, presigned URLs, and CLI
a03a0e6 feat: S3 XML serialization, HTTP handlers, and multipart upload manager
2f739cc feat: AWS SigV4 request verification and IAM-like policy engine
40ea1ea feat: SQLite metadata store and content-addressable object storage
896e576 feat: core domain types and YAML config loader
5f67b38 chore: scaffold cabal project and import prelude
```

---

## 6. Next Priorities

1. **P0**: Fix PUT authorization bypass — route through `handlePutObject`; add ETag to PUT responses
2. **P0**: Wire CopyObject, HeadObject, and Presigned URL validation into Server router
3. **P1**: Fix XML compliance — add namespace, ISO-8601 dates, NextMarker, isTruncated logic
4. **P1**: Implement ListObjectsV2 with continuation token
5. **P2**: Wire orphaned test files into Spec.hs; add integration tests for critical request path
