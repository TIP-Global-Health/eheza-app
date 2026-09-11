---
name: design-brief-backend-per-record-commit
description: "Design brief (poison-batch Option C) — per-record commit + per-record outcomes on /api/sync uploads, so one bad record no longer jams its shard. Facts verified on develop @91fb401fb, 2026-07-06."
metadata: 
  node_type: memory
  type: project
  originSessionId: 621f6a16-0139-4571-9899-6d1a21bb15f8
---

# Design brief — Per-record commit on sync upload (poison-batch "Option C")

**Status:** brief only, not approved. Companion to [[improvement-1b-poison-batch-not-quick-fix]] (why client-side skip is INVALID) and [[design-brief-sync-jam-visibility]] (Option A — visibility; complementary, do A first or together). Anchors verified on develop @91fb401fb (2026-07-06).

## Problem (current state, verified)

`HedleyRestfulSync::handleChanges` wraps the WHOLE 50-change client batch in one `db_transaction()` (:427-430); ANY exception (unknown type 400, `Could not find UUID:` 400, sub-handler validation, presave duplicate-UUID 500, DB error) rolls back everything and re-throws (:511-537). The client (both lanes) re-POSTs the identical FIFO batch every sync tick forever — one poison record blocks its shard's entire upload queue, including all records created later.

Key verified contract facts:
- Client sends `{changes: [{uuid, type, method, data}...], db_version}`, 50/batch (app.js:920/:994). FK refs travel as UUIDs in `data`; server converts per-item via `hedley_restful_uuid_to_nid` (throws on unknown).
- **Client ignores the upload response body entirely** (`WebData ()`, no decoder — Update.elm:1249-1255 comment is explicit). On HTTP 200 it deletes the WHOLE batch from IndexedDB by localId (`deleteEntitiesThatWereUploaded`, already list-shaped). On non-200: nothing deleted, retry.
- Idempotency primitives EXIST: POST with existing `field_uuid` is silently skipped (:496-507, "duplicate request"); presave `hedley_device_verify_unique_uuid` backstops races; PATCH re-applies (new revision each time — noisy but safe).
- Version gate: `validateDbVersion` hard-rejects clients below `HEDLEY_RESTFUL_CLIENT_SIDE_INDEXEDDB_SCHEMA_VERSION` (=32) — the existing force-upgrade lever. The sync endpoint itself is unversioned 1.0; the `files_upload__1.1.inc` minor-version pattern is the in-repo evolution precedent.
- Upload rows: `nodeChanges`/`shardChanges` (`++localId,...,isSynced`) — NO attempts/error/quarantine fields.

## ⚠ THE compat constraint (decisive for rollout)

If the server starts committing per-record and returns 200-with-outcomes while OLD clients are deployed, an old client will treat the 200 as full success and **delete all 50 local rows including the failed ones → the poison records are permanently lost client-side.** Therefore the server MUST NOT change semantics for old payloads. Two safe gates:
- (a) **Capability flag (recommended):** new clients send `"per_record": true` in the POST body; server runs per-record mode only when present, otherwise byte-identical legacy all-or-nothing. Zero forced upgrades, both modes co-exist.
- (b) Bump the db_version constant to force-upgrade all clients first, then flip. (Blunt: bricks un-upgraded devices until they update.)

## Design

**Server (`HedleyRestfulSync::handleChanges`), gated on the capability flag:**
1. Drop the outer transaction; wrap EACH item in its own `db_transaction()` + try/catch. On item failure: let the item's transaction roll back, record `{uuid, status: "failed", reason: <message>}`, `continue`. On success: `{uuid, status: "ok"}` (transaction commits on scope exit). FIFO order preserved — children of a failed parent naturally fail their own `uuid_to_nid` and are recorded failed too (they stay client-side, no FK ordering violated — this is what makes per-record VALID where client-side skip was not).
2. Response: `return ['outcomes' => $outcomes];` — additive; HTTP 200 whenever the request itself was well-formed. Keep 400 for malformed envelope / db_version.
3. Keep the UUID-resolve request-static cache semantics (positive-only) — unchanged; per-item rollback can't poison it (it only caches committed-or-in-batch positives; VERIFY at impl: a rolled-back item's created nid must not linger in the cache for later items — flush cache entry on rollback or re-resolve).
4. Widen incident creation: report `content-upload` incident per failed item (dedup already per device+identifier), not just the UUID-prefix case.

**Client:**
5. Add `"per_record": true` to the upload envelope; decode `outcomes` (additive decoder; absent field → legacy behavior). On 200: partition by uuid → map to localIds (client has the uuid↔localId pairing in the batch record) → `deleteEntitiesThatWereUploaded` with ONLY the ok subset (port already takes a list — no JS change). Failed rows stay `isSynced=0` and re-enter the next batch.
6. Lane status: if some records failed, do NOT mark sync info Success; keep/enter the jam-visible state from Option A (the two briefs compose: Option A detects "no progress", which after Option C means "genuinely-poison residue only").

**Known bounded limitation (accept + document):** failed rows keep their low localIds → they occupy batch-head slots forever. Progress continues while poison-count < 50 per shard; a person-rooted failure tree could theoretically accumulate to 50 over a long time → wedge returns. Mitigations if ever needed (defer): attempts counter column + skip-after-N into a quarantine flag surfaced to support (this is safe ONLY post-Option-C because dependents fail deterministically rather than blocking).

## Effort / phases

- Phase 1 server (M, ~2-3 days incl. simpletest coverage of mixed batches: ok+failed, failed-parent+child, duplicate retry).
- Phase 2 client (M, ~2 days: envelope flag, outcomes decoder + Msg payload, partitioned delete, tests in SyncManager/Test.elm fixture style; e2e sync happy-path must stay green).
- Ship server first (inert for old clients thanks to the flag), client second.

## Risks / mitigations

- Old-client data loss if gating is skipped → the capability flag is non-negotiable (review checklist item).
- Per-item transactions change failure atomicity for multi-node side-effects within ONE item (presave hooks enqueue AQ tasks etc.) — same scope as one item's node_save today inside the big transaction; VERIFY AQ enqueues of failed items roll back (they do today — same connection).
- Retry-after-timeout now interacts per-record: POST-skip + PATCH-reapply already make this safe (verified §idempotency).
- Watchdog noise per failed item every cycle → rate-limit logging per uuid (cheap guard) or rely on incident dedup.

## Open decisions (user)

1. Approve the capability-flag rollout (vs force-upgrade)?
2. Should PATCH-target-missing (`Could not find UUID` on the PATCH nid) be outcome-failed (stay queued) or outcome-ok-skipped (drop client-side)? Recommend failed (conservative).
3. Quarantine mechanism now or deferred? (Recommend deferred — bounded limitation documented.)
4. Do Option A first? (Recommended: visibility validates Option C's effect in production.)

## Acceptance criteria

Dev-env batch with 1 poison POST (unknown UUID ref) + 49 good: server commits 49, returns outcomes, client deletes 49, keeps 1, lane keeps cycling with fresh records; old-client simulation (no flag) gets legacy all-or-nothing behavior byte-identical to today; duplicate re-POST of a committed batch is a no-op; simpletest + e2e green.
