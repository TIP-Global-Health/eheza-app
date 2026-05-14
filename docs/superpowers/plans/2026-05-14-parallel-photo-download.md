# Parallel data + photo download — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the SyncManager photo-download lane run concurrently with the data-download lane, so deferred photos start downloading from the first entity batches instead of waiting for the entire ~500K-entity sync to settle.

**Architecture:** The `SyncManager` already has two independent timer-driven lanes (`BackendFetchMain` for data, `BackendFetchPhotos` for photos). A single guard in `determineDownloadPhotosStatus` pins the photo lane to idle whenever the data lane is active. This plan removes that guard — a single change to one function. The two lanes share no mutable model state, so no other changes are needed.

The post-sync photo-download kick (`SchedulePhotosDownload` → debounced `TryDownloadingPhotos`) is **intentionally kept** — see "Design note" below.

**Tech Stack:** Elm 0.19.1. Tests via `elm-test` (run per-file: `npx elm-test <path>` — `npx` so the project-pinned `elm-test@0.19.1-revision6` is used, not a newer global one). Linting via `elm-format` and `elm-review`.

**Spec:** `docs/superpowers/specs/2026-05-14-parallel-photo-download-design.md`

**Branch:** `parallel-photo-download` (off `bulk-photo-fetch`; spec and plan already committed on it).

---

## Design note: the post-sync kick is intentionally kept

An earlier draft of this plan removed `SchedulePhotosDownload` / `TryDownloadingPhotos` as "redundant" once the photo lane runs continuously. That removal was **dropped**. Rationale:

- Small HCs are the common case. Their sync completes fast, and the existing "after sync completed → trigger photo download" kick is the well-tested mechanism that gets photos going. Keeping it preserves that proven behaviour for the common path.
- The concern with keeping it — that the kick could fire while the photo lane is *already running* (now possible, since the lanes run in parallel) — is already handled. The `TryDownloadingPhotos` handler is guarded:

  ```elm
        TryDownloadingPhotos ->
            case model.downloadPhotosStatus of
                DownloadPhotosIdle ->
                    update ... BackendFetchPhotos model

                _ ->
                    -- Sync is already in progress.
                    noChange
  ```

  If the photo lane is in process (`downloadPhotosStatus` is anything but `DownloadPhotosIdle`), the kick is a clean `noChange` no-op — no double-start, no breakage.

So `SchedulePhotosDownload`, `TryDownloadingPhotos`, both dispatch sites, and the `debouncer` field are all left untouched. The only code change in this plan is the guard removal (Task 1).

---

## File Structure

- `client/src/elm/SyncManager/Utils.elm` — **modify.** `determineDownloadPhotosStatus` loses the `case model.syncStatus of …` guard wrapper.
- `client/src/elm/SyncManager/Test.elm` — **create.** First unit test for `SyncManager`; pins the guard-removal behaviour. Follows the repo's co-located `*/Test.elm` pattern (e.g. `App/Test.elm`).

`SyncManager/Model.elm` and `SyncManager/Update.elm` are **not** touched.

---

## Task 1: Remove the sync-active guard in `determineDownloadPhotosStatus`

**Status: DONE** — committed as `31dd43da7` (the steps below are kept as the executed record).

**Files:**
- Create: `client/src/elm/SyncManager/Test.elm`
- Modify: `client/src/elm/SyncManager/Utils.elm` (function `determineDownloadPhotosStatus`)
- Test: `client/src/elm/SyncManager/Test.elm`

### Context

`determineDownloadPhotosStatus` currently wraps its photo state-machine logic in `case model.syncStatus of SyncIdle -> <inner> ; _ -> DownloadPhotosIdle`. The `_ -> DownloadPhotosIdle` branch is the guard: while the data lane is doing anything (`syncStatus /= SyncIdle`), the photo lane is forced idle. Removing the wrapper makes the inner logic run unconditionally. The outer `syncCycleRotate` check (is sync enabled at all) stays.

- [x] **Step 1: Write the failing test**

Create `client/src/elm/SyncManager/Test.elm` with exactly this content:

```elm
module SyncManager.Test exposing (all)

import EverySet
import Expect
import RemoteData
import SyncManager.Model
    exposing
        ( DownloadPhotosStatus(..)
        , Flags
        , Model
        , Site(..)
        , SyncCycle(..)
        , SyncInfoStatus(..)
        , SyncStatus(..)
        , emptyModel
        )
import SyncManager.Utils exposing (determineDownloadPhotosStatus)
import Test exposing (Test, describe, test)


testFlags : Flags
testFlags =
    { syncInfoGeneral =
        { lastFetchedRevisionId = 0
        , lastSuccesfulContact = 0
        , remainingToUpload = 0
        , remainingToDownload = 0
        , deviceName = ""
        , status = NotAvailable
        , rollbarToken = ""
        , site = SiteUnknown
        , features = EverySet.empty
        }
    , syncInfoAuthorities = Nothing
    , batchSize = 100
    , syncSpeed =
        { idle = 3000
        , cycle = 50
        , offline = 10000
        }
    }


testModel : Model
testModel =
    emptyModel testFlags


all : Test
all =
    describe "SyncManager photo lane"
        [ test "determineDownloadPhotosStatus progresses the photo lane while the data lane is downloading" <|
            \() ->
                determineDownloadPhotosStatus
                    { testModel
                        | syncStatus = SyncDownloadAuthority RemoteData.NotAsked
                        , downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCycleOn
                    }
                    |> .downloadPhotosStatus
                    |> Expect.notEqual DownloadPhotosIdle
        , test "determineDownloadPhotosStatus keeps the photo lane idle when the sync cycle is paused" <|
            \() ->
                determineDownloadPhotosStatus
                    { testModel
                        | syncStatus = SyncIdle
                        , downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCyclePause
                    }
                    |> .downloadPhotosStatus
                    |> Expect.equal DownloadPhotosIdle
        ]
```

- [x] **Step 2: Run the test to verify it fails**

Run from `client/`:

```bash
cd client && npx elm-test src/elm/SyncManager/Test.elm
```

Expected: the **first** test FAILS — `Expect.notEqual DownloadPhotosIdle` fails because the guard pins `downloadPhotosStatus` to `DownloadPhotosIdle` when `syncStatus = SyncDownloadAuthority …`. The **second** test PASSES (it is a regression guard for the `SyncCyclePause` behaviour, which this change does not touch).

If `elm-test` errors before running (compilation/dependency error rather than a test failure), `rm -rf client/elm-stuff` and re-run.

- [x] **Step 3: Remove the guard**

In `client/src/elm/SyncManager/Utils.elm`, replace the body of `determineDownloadPhotosStatus`. Find this block:

```elm
    if syncCycleRotate then
        let
            statusUpdated =
                case model.syncStatus of
                    SyncIdle ->
                        -- Cases are ordered by the cycle order.
                        let
                            currentStatus =
                                model.downloadPhotosStatus
                        in
                        case currentStatus of
                            DownloadPhotosIdle ->
                                DownloadPhotosInProcess model.downloadPhotosMode

                            DownloadPhotosInProcess record ->
                                case record of
                                    DownloadPhotosNone ->
                                        DownloadPhotosIdle

                                    DownloadPhotosBatch deferredPhoto ->
                                        if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                            -- We tried to fetch deferred photos from IndexDB,
                                            -- but there we non matching the query.
                                            DownloadPhotosIdle

                                        else if deferredPhoto.batchCounter < 1 then
                                            -- We've reached the end of the batch, so we
                                            -- need to rotate.
                                            DownloadPhotosIdle

                                        else
                                            currentStatus

                                    DownloadPhotosAll deferredPhoto ->
                                        if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                            -- We tried to fetch deferred photos from IndexDB,
                                            -- but there we non matching the query.
                                            DownloadPhotosIdle

                                        else
                                            -- There are still deferred photos in IndexDB
                                            -- that match out query.
                                            currentStatus

                    -- When sync is active, we stop photos download.
                    _ ->
                        DownloadPhotosIdle
        in
        { model | downloadPhotosStatus = statusUpdated }

    else
        -- No change.
        model
```

Replace it with this block (the inner `case currentStatus of …` is lifted out one level; `currentStatus` becomes a top-level `let` binding; the `case model.syncStatus of … _ -> DownloadPhotosIdle` wrapper is gone):

```elm
    if syncCycleRotate then
        let
            currentStatus =
                model.downloadPhotosStatus

            statusUpdated =
                -- Cases are ordered by the cycle order.
                case currentStatus of
                    DownloadPhotosIdle ->
                        DownloadPhotosInProcess model.downloadPhotosMode

                    DownloadPhotosInProcess record ->
                        case record of
                            DownloadPhotosNone ->
                                DownloadPhotosIdle

                            DownloadPhotosBatch deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    DownloadPhotosIdle

                                else if deferredPhoto.batchCounter < 1 then
                                    -- We've reached the end of the batch, so we
                                    -- need to rotate.
                                    DownloadPhotosIdle

                                else
                                    currentStatus

                            DownloadPhotosAll deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    DownloadPhotosIdle

                                else
                                    -- There are still deferred photos in IndexDB
                                    -- that match out query.
                                    currentStatus
        in
        { model | downloadPhotosStatus = statusUpdated }

    else
        -- No change.
        model
```

- [x] **Step 4: Run the test to verify it passes**

Run from `client/`:

```bash
cd client && npx elm-test src/elm/SyncManager/Test.elm
```

Expected: BOTH tests PASS.

- [x] **Step 5: Run elm-format on the touched files**

Run from the repo root:

```bash
elm-format --validate client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm
```

Expected: no output (both files are correctly formatted). If it reports a diff, run `elm-format client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm` to auto-format, then re-run `npx elm-test src/elm/SyncManager/Test.elm` to confirm tests still pass.

- [x] **Step 6: Commit**

```bash
git add client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm
git commit -m "Remove sync-active guard so photo lane runs in parallel" # + body, Issue #1741, [ci skip]
```

---

## Task 2: Full baseline lint + test verification

**Files:** none modified (verification only; commit only if `elm-format` reformats something).

### Context

Task 1 ran targeted `elm-format` and test checks. This task runs the full baseline the spec calls for, to catch anything the targeted checks missed.

- [ ] **Step 1: Validate formatting across the whole source tree**

Run from the repo root:

```bash
elm-format --validate client/src/
```

Expected: no output. If it reports any file, run `elm-format client/src/` to fix, then re-run the validate command.

- [ ] **Step 2: Run elm-review**

Per the repo's known caveat, clear the stale cache first. Run from the repo root:

```bash
rm -rf client/elm-stuff && cd client && elm-review
```

Expected: `I found no errors!`. Note: `SchedulePhotosDownload` and `TryDownloadingPhotos` are intentionally retained and still referenced, so there is no unused-constructor concern.

- [ ] **Step 3: Run the SyncManager unit tests**

Run from `client/`:

```bash
cd client && npx elm-test src/elm/SyncManager/Test.elm
```

Expected: both tests PASS.

- [ ] **Step 4: Compile the full app**

Run from `client/`:

```bash
cd client && elm make src/elm/Main.elm --output=/dev/null
```

Expected: `Success!`

- [ ] **Step 5: Commit any formatting fixes (only if Step 1 changed files)**

If and only if Step 1 reformatted files:

```bash
git add client/src/
git commit -m "$(cat <<'EOF'
Apply elm-format

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>

[ci skip]
EOF
)"
```

If Step 1 produced no changes, skip this step — do not create an empty commit.

---

## Task 3: Manual throttled-network verification

**Files:** none modified (manual verification; documented here so it is not skipped).

### Context

This is the load-bearing verification from the spec. The change knowingly re-opens the behaviour that PR #2057 ("Resolve initial sync problems on slow devices", 2020) removed, so the verification deliberately reproduces slow-device conditions. This task cannot be automated; it requires a running app, a populated backend, and Chrome DevTools. If the engineer executing this plan cannot run the app against a large health center, they must hand this task back to the user rather than mark it complete.

- [ ] **Step 1: Build and serve the app**

From the repo root, with DDEV running:

```bash
ddev gulp
```

This serves the app on `localhost:3000` and watches for changes.

- [ ] **Step 2: Pair the device and trigger an initial sync against a large health center**

Open `localhost:3000`, pair the device (default pairing code `12345678`, nurse PIN `1234`), and select a health center with a large dataset so the initial sync runs long enough to observe (the data lane stays non-idle for an extended period).

- [ ] **Step 3: Throttle the network**

Open Chrome DevTools → Network tab → throttling dropdown → select **Slow 3G**. Keep the Network tab open for the rest of the verification.

- [ ] **Step 4: Confirm the checks**

While the initial sync runs (and after it settles), confirm each of these. Record the result for each:

1. The data lane progresses and the sync eventually completes — `syncStatus` reaches `SyncIdle`, the sync does not hang.
2. Bulk-photo requests (`POST` to the bulk-photos endpoint) appear in the Network tab **interleaved** with the `/api/sync` requests, starting early in the entity download — not only after the entity download finishes.
3. Neither lane hangs or stalls.
4. The `>45s`-sync page refresh still fires (the app reloads itself after the long sync settles).
5. After that refresh, the photo lane resumes — bulk-photo requests continue draining `deferredPhotos` post-reload.
6. **Post-sync kick is harmless when photos are already running.** On a *small* HC (fast sync, where the post-sync `SchedulePhotosDownload` kick fires while the photo lane may already be in process), confirm there is no double-fetch, no error, and no stall — the kick is absorbed cleanly. (This is the scenario `TryDownloadingPhotos`' `noChange` guard protects; this step confirms it in practice.)

- [ ] **Step 5: Record the outcome**

If all checks pass, the feature is verified — note this in the PR description. If any check fails, do **not** proceed to merge; capture the failure (which check, what happened, screenshots of the Network tab) and report back — this is the slow-device regression the spec warned about, and it needs investigation before this ships.

---

## Self-Review Notes

- **Spec coverage:** Task 1 covers the guard removal (spec §The changes). Task 2 covers the automated baseline (spec §Testing). Task 3 covers the manual throttled-network test (spec §Testing, the load-bearing verification). Branching is done — branch `parallel-photo-download` exists off `bulk-photo-fetch` with the spec and plan committed.
- **Post-sync kick kept by design:** an earlier draft removed `SchedulePhotosDownload` / `TryDownloadingPhotos`; that was dropped on review — the kick is the tested mechanism that starts photos after a fast small-HC sync, and `TryDownloadingPhotos` already no-ops when the photo lane is in process. See "Design note" above. The plan's only code change is Task 1's guard removal.
- **No Model.elm field change:** the spec's earliest draft proposed splitting `downloadRequestTime`; investigation confirmed the photo lane never touches that field (it tracks in-flight state via `backendRemoteData` inside `downloadPhotosStatus`), so no field split is needed. The spec was corrected accordingly.
- **`Issue #1741`** is used in commit messages per the repo convention of linking issues without `Closes`/`Fixes`. Confirm 1741 is the correct issue for this work; if a different issue tracks the parallel-download work, substitute it.
