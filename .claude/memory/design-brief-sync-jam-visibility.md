---
name: design-brief-sync-jam-visibility
description: "Design brief (poison-batch Option A) — make a jammed upload sync lane visible to nurses and admins; skip nothing, lose nothing. Facts verified on develop @91fb401fb, 2026-07-06."
metadata: 
  node_type: memory
  type: project
  originSessionId: 621f6a16-0139-4571-9899-6d1a21bb15f8
---

# Design brief — Sync upload-jam visibility (poison-batch "Option A")

**Status:** brief only, not approved for implementation. Companion to [[improvement-1b-poison-batch-not-quick-fix]] (why client-side skip is INVALID) and [[design-brief-backend-per-record-commit]] (Option C). All anchors verified on develop @91fb401fb (2026-07-06).

## Problem (current state, verified)

One server-rejected record jams its shard's whole upload batch: the batch POST to `/api/sync` is transactional server-side (rollback on any exception), and the client retries the SAME batch every ≥10s forever (`getSyncSpeedForSubscriptions` failure cadence; `FetchFromIndexDbUploadAuthority` wipes the previous `Failure` each tick). `determineSyncStatus` advances upload lanes ONLY on `indexDbRemoteData == Success Nothing` (empty batch) — `backendRemoteData = Failure` hits `_ -> noChange`. Content lanes (General/Authority/WhatsApp) have NO errorsCount/threshold — unlike photo/screenshot (`SyncUploadPhoto Int …`, `fileUploadFailureThreshold = 5`) and unlike `bulkPhotosConsecutiveBatchErrors` (3-strikes precedent).

What a nurse sees today: Device page only — "Status: Error" flickering back to "Uploading" every ~10s (raw untranslated string), stale `remainingToUpload` (written only on upload success), and `lastSuccesfulContact` that keeps ADVANCING (it's download-only) — nothing says records are stuck, which, or that it won't self-heal. No global banner (the storage-full banner in `App/View.elm viewStorageWarning` is the only global-chrome alert). `viewBySyncStatus` treats `Error` as normal content.

Admin side today: server creates a `sync_incident` node + email (`hedley_restful_report_sync_incident`, deduped per device+type+identifier, mail to `hedley_restful_incident_notifiers_list` var, default tip-incident@gizra.com) — but ONLY for exceptions starting `Could not find UUID:`. Validation errors/500s → rollback + watchdog, no incident. No admin View lists sync_incident nodes; the only surface is the static `admin/reports/devices-sync-state` table (requires an incident node + total_to_upload>10 + phase sync-start).

## Goals / non-goals

- GOALS: (1) nurse sees a persistent, translated, non-technical warning when uploads are jammed; (2) admin/support get an incident (node+email) for ANY persistent jam, not just UUID-poison; (3) the jam's first-failing batch head is identifiable for support. ZERO change to retry/data semantics — skip nothing, lose nothing.
- NON-GOALS: unjamming (that's Option C), client-side skip/quarantine (INVALID — FIFO+FK), retry back-off tuning.

## Design

1. **Track upload failure state (client, Model):**
   - `uploadConsecutiveFailures : Int` in SyncManager Model (NOT inside the SyncStatus variant — avoids type churn); increment in `BackendUploadAuthorityHandle`/`BackendUploadGeneralHandle` Failure branches, reset to 0 on any upload Success.
   - `lastUploadSuccess : Int` (millis) + `lastUploadError : Maybe String` persisted alongside syncInfo (localStorage round-trip via the existing syncInfo ports/flags — adding fields needs tolerant decoding for old stored JSON; precedent: statsCacheHash was added later).
2. **Jam predicate** (pure helper in Utils, unit-testable): jammed = `uploadConsecutiveFailures >= threshold` OR (`now - lastUploadSuccess > window` AND `remainingToUpload > 0` AND `lastSuccesfulContact` recent — the last condition proves the device is online, since downloads still succeed during a jam). Suggested: threshold 10 (~2 min), window 4h. Both values as constants next to `fileUploadFailureThreshold`.
3. **Nurse-facing banner:** extend `viewStorageWarning` → `viewSyncWarning` in the same global chrome slot ("ui warning message"): "Some records on this device cannot reach the server. Your data is safe on the device. Please contact support." (new TranslationSet; English + fallbacks, native fills later per R2-5 process). Device page: replace the flickering raw string with a sticky state while jammed — "Upload blocked since <time>" + the persisted `lastUploadError`.
4. **Client incident report:** add `ContentUploadIncident IncidentContnentIdentifier` variant to `SyncIncidentType` (alphabetical), encoder emits `incident_type = "content-upload"` — **backend-compatible TODAY** (allowed_values already `files-upload`/`content-upload`; note the pre-existing mismatch: Elm's file incident sends "file-upload", off-list — fix opportunistically). Trigger: on jam-predicate flip, enter the existing `SyncReportIncident` status once (server dedups per device+identifier; identifier = shard uuid + head-of-batch localId/uuid). Include the first batch entity's uuid in details via the existing `/api/report-incident-details` path.
5. **Server-side widening (small PHP):** in `HedleyRestfulSync` catch, create the incident for ANY exception (not just `Could not find UUID:` prefix) — identifier = item uuid, details = exception message. Dedup already prevents mail storms.
6. **Admin View (optional phase 2):** a Views listing of `sync_incident` nodes (date, device, type, identifier, details) + link from `admin/reports/devices-sync-state`.

## Effort / phases

- Phase 1 (client visibility, ~1–2 days): items 1–3. Elm only + translations. Port/flags schema addition is the fiddly part.
- Phase 2 (incidents, ~0.5–1 day): items 4–5. Small Elm + small PHP.
- Phase 3 (admin View, ~0.5 day): item 6. Features-exported View.

## Risks / mitigations

- syncInfo localStorage schema change → tolerant decoder defaults (old JSON lacks new fields); verify flags decode path on upgrade.
- Banner fatigue / false positives on genuinely-offline devices → the "downloads still succeed" condition gates it; tune window.
- New translations: English-only initially (runtime falls back), flag for native fill — do NOT machine-fill (R2-5 policy).

## Open decisions (user)

1. Threshold/window values (10 failures / 4h suggested).
2. Banner on all pages (like storage-full) vs Device-page-only?
3. Phase 2/3 in scope?
4. Fix the "file-upload"→"files-upload" off-list value while touching the encoder? (needs care: server dedup keys on the string; changing it creates one duplicate incident per device max).

## Acceptance criteria

Simulated persistent 500 on `/api/sync` (dev env): banner appears within ~2 min and persists across reload; Device page shows sticky "blocked since" + error; exactly ONE sync_incident node + email per device (deduped); retry behavior byte-identical to today; all Elm gates green (make/format/review/test incl. a unit test on the jam predicate).
