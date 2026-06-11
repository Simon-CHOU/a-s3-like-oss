# CLAUDE.md

## Project Overview

s3-oss is a self-contained, single-binary, S3-compatible object storage service written in Haskell. It speaks the S3 REST API over HTTPS, authenticates requests via AWS Signature Version 4 (SigV4), enforces IAM-like authorization policies, and stores objects on the local filesystem using content-addressable storage. Features include multipart uploads, presigned URLs, and streaming object I/O via conduit.

**Tech Stack:** GHC 2021 / GHC 9.6+, cabal, servant ^>=0.20, warp ^>=3.3, wai ^>=3.2, crypton ^>=1.0, conduit ^>=1.3, sqlite-simple ^>=0.4, xml-conduit ^>=1.9, rio ^>=0.1, optparse-applicative, hspec ^>=2.11, QuickCheck ^>=2.14

**Maturity:** Alpha (5.2/10) -- builds successfully, architectural skeleton is sound, but critical protocol-compliance defects (see Known Gaps) prevent use with real S3 clients.

## Build & Test Commands

- **Build:** `cabal build`
- **Run tests:** `cabal test` (runs 3 of 6 spec modules; 3 test files are orphaned)
- **Run single test match:** `cabal test --test-option='-m' --test-option='Policy'`
- **Start dev server:** `cabal run s3-oss -- --dev --port 9000 --data-dir ./data`
- **Start with config:** `cabal run s3-oss -- --config /path/to/config.yaml`
- **Optimized build:** `cabal build --enable-optimization=2`

## Architecture

### Module Hierarchy

```
Foundation
  S3OSS.Prelude    -- Custom prelude re-exporting RIO + text/byteString helpers
  S3OSS.Types      -- Core domain types (Buckets, Objects, Policy, Multipart, etc.)
  S3OSS.Config     -- YAML config loading, CLI override resolution

Data
  S3OSS.Store      -- SQLite metadata persistence (buckets, objects, multipart state)

Auth
  S3OSS.Auth.SigV4    -- AWS Signature Version 4 verification
  S3OSS.Auth.Policy   -- IAM-like policy evaluation engine (Allow/Deny, ARN matching)
  S3OSS.Presigned     -- Presigned URL generation (validation not yet wired into Server)

Storage
  S3OSS.Object.Storage -- Content-addressable filesystem storage (put/get/delete via conduit)

Serialization
  S3OSS.XML        -- S3 XML response rendering and request body parsing (xml-conduit)

Handlers
  S3OSS.Bucket.Handler     -- CreateBucket, DeleteBucket, ListBuckets, HeadBucket
  S3OSS.Object.Handler     -- PutObject, GetObject, DeleteObject, HeadObject, CopyObject
  S3OSS.List.Handler       -- ListObjects (V1 only; V2 not implemented)
  S3OSS.Multipart.Manager  -- Multipart state machine, part assembly, GC
  S3OSS.Multipart.Handler  -- CreateMultipartUpload, UploadPart, Complete, Abort

Integration
  S3OSS.Server     -- WAI Application, manual routing, middleware stack, server startup
  app/Main.hs      -- CLI argument parsing (optparse-applicative), config loading
```

### Request Data Flow

```
HTTP Request
  -> Warp (TLS termination)
    -> WAI Application (manual method+path routing)
      -> resolveUser (SigV4 auth or dev-mode bypass)
        -> Policy evaluation (IAM-style Allow/Deny)
          -> Handler (business logic)
            -> Store (SQLite metadata R/W)
            -> Object.Storage (content-addressable filesystem I/O via conduit)
          -> S3 XML Response (xml-conduit Document -> lazy ByteString)
```

### Content-Addressable Storage Layout

```
<data-dir>/
  meta.sqlite                              -- All metadata (SQLite WAL mode)
  objects/
    <sha256[0:2]>/                         -- First 2 hex chars as shard directory
      <full-sha256-hex>                    -- Full SHA-256 hex = filename (final, immutable)
  multipart/
    <upload-id>/                           -- Per-upload temp directory
      part-<NNNNN>                         -- Individual part files (zero-padded 5-digit)
```

**Properties:** content-addressable (same content = same hash = single copy), atomic writes via temp-file-then-rename, immutable once written, deduplication by hash, integrity via incremental SHA-256 + MD5 during streaming writes.

## Programming Discipline

### Ultracode & Dynamic Workflows

- The "ultracode" keyword enables multi-agent Workflow orchestration scripts.
- Workflows use 3-4 named phases with pipeline() for fan-out, parallel() for barriers.
- Schema objects (JSON Schema) must be declared as const at the TOP of the script (JS TDZ).
- Workflow scripts are persisted and can be resumed with scriptPath + resumeFromRunId.
- agent() calls use label (display), phase (grouping), schema (structured output).
- pipeline() is DEFAULT for multi-stage work (no barrier between stages).
- parallel() only when genuinely needing ALL results together (dedup, early-exit).
- Typical workflow: Audit (pipeline read+analyze) -> Fix (sequential in dep order) -> Verify (build) -> Integrate (cross-module).

### Report Logs (docs/report-YYYYMMDD-HHMMSS.log)

- Created at major milestones: after build success, after test pass, after maturity assessment.
- Header format: E2E UAT Report with timestamp and goal statement.
- Contains: status summary, blocker list, module breakdown, next actions.
- Purpose: snapshot of project health at a point in time.

### Spike Logs (docs/spike-YYYYMMDD-HHMMSS.log)

- Created before complex work: compilation analysis, blocker investigation, pre-build checks.
- Header format: Spike Report with timestamp and focus area.
- Contains: specific findings, error counts, root cause analysis, fix recommendations.
- Purpose: focused deep-dive on a single problem area before committing to fixes.

### Design Specs (docs/superpowers/specs/)

- Comprehensive design document before implementation.
- Sections: overview, feature scope, architecture, data models, dependencies, testing strategy, config.
- Explicit non-goals section to bound scope.
- Current spec: `docs/superpowers/specs/2026-06-11-s3-like-oss-design.md`.

### Implementation Plans (docs/superpowers/plans/)

- Step-by-step task breakdown with checkbox tracking.
- Each task: files to create/modify, code snippets, verification command, commit message.
- Tasks ordered by dependency (scaffolding -> types -> data -> auth -> handlers -> integration).
- Current plan: `docs/superpowers/plans/2026-06-11-s3-oss-implementation.md`.

### Maturity Reports (docs/maturity-report-YYYYMMDD.md)

- Updated after each major workflow pass.
- Module-by-module scores: correctness, completeness, S3 compliance, test coverage (each 1-10).
- Layer averages, critical gaps ranked by severity, production readiness assessment.
- Next priorities with P0/P1/P2 ranking.
- Current report: `docs/maturity-report-20260612.md`.

### Git Commit Discipline

- Conventional commits: chore/feat/fix/docs prefix.
- Logical layering: each commit is one coherent layer (scaffolding -> types -> data -> auth -> handlers -> integration).
- Commit messages describe WHAT and WHY, not just what files changed.
- Each commit should leave the project buildable.
- After workflow-driven fixes, a single "fix:" commit captures all audit-driven changes.

### /loop Usage

- /loop is used for autonomous iterative development cycles.
- Each loop iteration: assess current state -> identify next action -> execute -> verify -> report.
- Loops terminate when: all tests pass, build succeeds, or explicit stop condition met.

## Module Map

### Source Modules (15 total, under src/S3OSS/)

| Module | Lines | Export List | Description |
|--------|-------|-------------|-------------|
| S3OSS.Prelude | 14 | Explicit (re-export) | Custom prelude wrapping RIO + text/ByteString encodings |
| S3OSS.Types | 153 | None (exports all) | Core domain types: Sha256Hex, ETag, BucketName, ObjectKey, AccessKey, SecretKey, Action, Effect, Policy, User, ObjectInfo, BucketInfo, UploadState, MultipartUpload, PartInfo, OwnerInfo |
| S3OSS.Config | 161 | None (exports all) | YAML config loading, policy resolution, default dev config |
| S3OSS.Store | 321 | None (exports all) | SQLite metadata store: bucket/object CRUD, multipart operations, upload GC, ISO-8601 parsing |
| S3OSS.XML | 153 | None (exports all) | S3 XML serialization (ListBuckets, Error, InitiateMultipart, CompleteMultipart, CopyObject, ListObjects) and request parsing (CompleteMultipart body) |
| S3OSS.Auth.SigV4 | 160 | Explicit | AWS SigV4 signature verification: key derivation, canonical request building, header parsing, verifySigV4 |
| S3OSS.Auth.Policy | 96 | Explicit (evaluate) | IAM-like policy evaluation: ARN wildcard matching, deny-overrides-allow, S3AllActions wildcard |
| S3OSS.Object.Storage | 79 | Explicit (putObject, getObject, deleteObject) | Content-addressable filesystem storage: streaming put with SHA-256+MD5, hash-based get/delete |
| S3OSS.Bucket.Handler | 85 | Explicit | Bucket HTTP handlers: createBucket, deleteBucket, listBuckets, headBucket with policy checks |
| S3OSS.Object.Handler | 141 | Explicit | Object HTTP handlers: putObject, getObject, deleteObject, headObject, copyObject with policy checks |
| S3OSS.List.Handler | 43 | Explicit (handleListObjects) | ListObjects V1 handler: prefix/delimiter/maxKeys, pagination structure (isTruncated always False -- known bug) |
| S3OSS.Multipart.Manager | 171 | None (exports all) | Multipart upload state machine: generateUploadId, initiateUpload, uploadPart, completeUpload (streaming assembly), abortUpload, background GC |
| S3OSS.Multipart.Handler | 86 | None (exports all) | Multipart HTTP handlers: create/uploadPart/complete/abort with policy checks |
| S3OSS.Presigned | 51 | None (exports all) | Presigned URL generation for GetObject/PutObject (non-standard signing scheme; validation not wired into server) |
| S3OSS.Server | 217 | None (exports all) | WAI Application: manual method+path routing, SigV4/dev-mode auth, Warp/TLS startup |

### Test Modules (6 total, under test/)

| Module | Wired in Spec.hs | Description |
|--------|-----------------|-------------|
| S3OSS.XMLSpec | Yes | XML serialization round-trip and parsing tests |
| S3OSS.Auth.PolicySpec | Yes | Policy evaluation tests: allow, deny-overrides, wildcard action/resource, prefix/exact ARN matching |
| S3OSS.Object.StorageSpec | Yes | Storage engine tests: write/read round-trip, empty objects, content deduplication |
| S3OSS.Auth.SigV4Spec | **No (orphaned)** | SigV4 signing key derivation test (32-byte output check) |
| S3OSS.Bucket.HandlerSpec | **No (orphaned)** | Placeholder (1+1=2) -- integration tests pending hspec-wai setup |
| S3OSS.Multipart.ManagerSpec | **No (orphaned)** | Placeholder (1+1=2) -- integration tests pending |

### Export List Status

- **Explicit export lists:** Auth.Policy, Object.Storage, List.Handler, Bucket.Handler, Object.Handler, Auth.SigV4, Prelude (re-export)
- **Missing (exports all):** Types, XML, Config, Store, Server, Multipart.Manager, Multipart.Handler, Presigned

## Known Gaps (P0 Items)

1. **CopyObject not routed** -- handleCopyObject exists in Object.Handler but Server.hs does not route the CopyObject path (PUT with x-amz-copy-source header); the routing code exists in the current Server.hs but needs verification.
2. **Presigned URL validation not wired** -- presignUrl generates URLs but the server has no presigned URL validation middleware.
3. **ListObjectsV2 not implemented** -- Only ListObjects V1 exists; V2 (continuation tokens, list-type=2) is absent.
4. **XML namespace missing** -- All XML responses lack `xmlns="http://s3.amazonaws.com/doc/2006-03-01/"`.
5. **Pagination broken** -- isTruncated always set to False in ListObjects handler logic.
6. **3 orphaned test files** -- SigV4Spec, Bucket/HandlerSpec, Multipart/ManagerSpec exist but are not imported in test/Spec.hs.
7. **PUT response lacks ETag** -- Clients expect ETag header on successful PUT responses.
8. **Prelude underutilized** -- Only 4 of 15 modules import S3OSS.Prelude; most import RIO directly.
9. **errorResponse duplicated** -- Identical errorResponse function is copy-pasted across Server.hs, Bucket.Handler, Object.Handler, List.Handler, and Multipart.Handler.

## Code Style

- **Language standard:** GHC2021.
- **GHC warnings (enforced via cabal common-opts):** `-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wpartial-fields -Wredundant-constraints`.
- **Prelude:** RIO re-exported through `S3OSS.Prelude` which adds `Data.ByteString`, `Data.ByteString.Lazy`, `Data.Text.Encoding`, `Data.Text.Encoding.Error` qualified imports.
- **OverloadedStrings** enabled in all modules.
- **Error handling:** Prefer `Either Text` over exceptions for recoverable errors (see Config.resolveConfig, Store.createBucket).
- **Partial functions:** Avoid `head`, `fromJust`; use `Maybe` and `listToMaybe` instead.
- **Streaming I/O:** Use conduit for all streaming (request body ingestion, object read/write, multipart assembly). Never load entire objects into memory.
- **XML:** Use `xml-conduit` (Text.XML module). Do NOT use blaze-markup for XML rendering. Render via `X.renderLBS X.def`.
- **Text over String:** Use `RIO.Text` and `Data.Text` throughout. Use `tshow` for Text formatting. Only use `String` for file paths and select library APIs.
- **Naming conventions:** Handler functions prefixed with `handle`. Helper/private functions lower camelCase. Types UpperCamelCase. Newtypes wrap single field with `un` prefix unwrapper.
- **Imports:** Group as: RIO / RIO sub-modules / standard library / third-party / project modules. Prefer qualified imports for non-RIO packages. Keep imports at top of file (not scattered or bottom-of-file).
