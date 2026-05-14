# Parallel data + photo download — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the SyncManager photo-download lane run concurrently with the data-download lane, so deferred photos start downloading from the first entity batches instead of waiting for the entire ~500K-entity sync to settle.

**Architecture:** The `SyncManager` already has two independent timer-driven lanes (`BackendFetchMain` for data, `BackendFetchPhotos` for photos). Three changes: (1) remove the guard in `determineDownloadPhotosStatus` that pins the photo lane to idle while the data lane is active; (2) kick the photo lane out of idle when deferred-photo rows land in IndexedDB, so it does not wait out its slow (10-minute default) idle timer; (3) fix the now-misleading "Idle, waiting for next Sync cycle" message on the Device page's photos-transfer status panel. The two lanes share no mutable model state.

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

So `SchedulePhotosDownload`, the `debouncer` field, and the post-sync dispatch sites are left untouched. `TryDownloadingPhotos` is kept too — Task 2 gives it a second caller (the deferred-photos save handler).

---

## File Structure

- `client/src/elm/SyncManager/Utils.elm` — **modify.** `determineDownloadPhotosStatus` loses the `case model.syncStatus of …` guard wrapper.
- `client/src/elm/SyncManager/Update.elm` — **modify.** `SavedAtIndexDbHandle` gains a case for `IndexDbSaveResultTableDeferredPhotos` that dispatches `TryDownloadingPhotos`.
- `client/src/elm/SyncManager/Test.elm` — **create.** First unit tests for `SyncManager` — the guard-removal behaviour and the deferred-photos kick wiring. Follows the repo's co-located `*/Test.elm` pattern (e.g. `App/Test.elm`).
- `client/src/elm/Translate.elm` — **modify.** Rename the `IdleWaitingForSync` translation key to `PhotosTransferIdle` and update its text.
- `client/src/elm/Pages/Device/View.elm` — **modify.** `viewPhotosTransferInfo` uses the renamed key.

`SyncManager/Model.elm` is **not** touched.

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

## Task 2: Kick the photo lane when deferred-photo rows land in IndexedDB

**Status: DONE** — committed as `233812bee`.

**Files:**
- Modify: `client/src/elm/SyncManager/Update.elm` (handler `SavedAtIndexDbHandle`)
- Test: `client/src/elm/SyncManager/Test.elm`

### Context

Task 1 lets the photo lane *progress* during sync, but it does not *bootstrap* it. The lane only leaves `DownloadPhotosIdle` on a `BackendFetchPhotos` timer tick, and that timer's idle interval is `getDownloadPhotosSpeedForSubscriptions`, which returns `syncSpeed.idle` verbatim. `syncSpeed.idle` defaults to **10 minutes** (`client/src/js/app.js`, `getSyncSpeed`). So after Task 1 alone, photos still took *minutes* to start during a sync — confirmed by manual testing (photos started during sync, but minutes late).

The fix: deferred-photo rows are written via `sendSyncedDataToIndexDb {table = "DeferredPhotos", ...}`; the JS side confirms the save through the `savedAtIndexedDb` port, which Elm receives as `SavedAtIndexDbHandle` carrying an `IndexDbSaveResult` whose `table` is `IndexDbSaveResultTableDeferredPhotos`. That case currently falls through to `_ -> noChange`. Adding an explicit branch that dispatches `TryDownloadingPhotos` kicks the photo lane out of idle the moment the rows have landed — race-free, and through a single point that covers every deferred-photo write path.

- [x] **Step 1: Add the failing test**

Append a third test to `client/src/elm/SyncManager/Test.elm`, plus the imports and `testDevice` fixture it needs. New imports: `Device.Model exposing (Device)`, `Json.Encode`, `Pages.Page exposing (Page(..))`, `SyncManager.Update`, `Time`, and `Msg(..)` added to the `SyncManager.Model` exposing list. New fixture:

```elm
testDevice : Device
testDevice =
    { accessToken = ""
    , refreshToken = ""
    , backendUrl = ""
    , deviceId = Nothing
    }
```

The test drives `SyncManager.Update.update` with a `SavedAtIndexDbHandle` message carrying a successful `DeferredPhotos` save result and asserts the photo lane has left `DownloadPhotosIdle`:

```elm
        , test "SavedAtIndexDbHandle for a successful DeferredPhotos save kicks the photo lane out of idle" <|
            \() ->
                let
                    saveResult =
                        Json.Encode.object
                            [ ( "table", Json.Encode.string "DeferredPhotos" )
                            , ( "status", Json.Encode.string "Success" )
                            , ( "timestamp", Json.Encode.string "" )
                            ]
                in
                SyncManager.Update.update
                    (Time.millisToPosix 0)
                    DevicePage
                    0
                    testDevice
                    (SavedAtIndexDbHandle saveResult)
                    { testModel
                        | downloadPhotosStatus = DownloadPhotosIdle
                        , syncCycle = SyncCycleOn
                    }
                    |> .model
                    |> .downloadPhotosStatus
                    |> Expect.notEqual DownloadPhotosIdle
```

- [x] **Step 2: Run the test to verify it fails**

`cd client && npx elm-test src/elm/SyncManager/Test.elm` — the new test FAILS (`SavedAtIndexDbHandle` for `DeferredPhotos` hits `_ -> noChange`, so the lane stays `DownloadPhotosIdle`); the other two PASS.

- [x] **Step 3: Add the `IndexDbSaveResultTableDeferredPhotos` case**

In `SavedAtIndexDbHandle` (`SyncManager/Update.elm`), between the `IndexDbSaveResultTableAutority` and `IndexDbSaveResultTableGeneral` branches, add:

```elm
                                IndexDbSaveResultTableDeferredPhotos ->
                                    -- Deferred-photo rows have just landed in IndexedDB.
                                    -- Kick the photo lane so it starts draining them now
                                    -- rather than waiting out its idle timer.
                                    update
                                        currentTime
                                        activePage
                                        dbVersion
                                        device
                                        TryDownloadingPhotos
                                        model
```

The `_ -> noChange` now covers only `IndexDbSaveResultTableAuthorityStats`.

- [x] **Step 4: Run the test to verify it passes**

`cd client && npx elm-test src/elm/SyncManager/Test.elm` — all 3 tests PASS.

- [x] **Step 5: elm-format + full compile**

`elm-format --validate` on the touched files (clean), then `elm make src/elm/Main.elm --output=/dev/null` (the fix touches `Update.elm`) — compiles, exit 0.

- [x] **Step 6: Commit**

```bash
git add client/src/elm/SyncManager/Update.elm client/src/elm/SyncManager/Test.elm
git commit -m "Kick photo lane when deferred-photo rows land in IndexedDB" # + body, Issue #1743, [ci skip]
```

---

## Task 2b: Fix the idle photos-transfer status message

**Status: DONE** — committed as `83c91af86`.

**Files:**
- Modify: `client/src/elm/Translate.elm`, `client/src/elm/Pages/Device/View.elm`

### Context

Found during review of Task 1. The Device page's "Photos Transfer Status" panel (`viewPhotosTransferInfo`) shows `Translate.IdleWaitingForSync` — "Idle, waiting for next Sync cycle" — when `downloadPhotosStatus` is `DownloadPhotosIdle`. That message described the *old* guarded behaviour (the lane parked behind sync). After Task 1, an idle photo lane just means "nothing in progress right now", not "blocked on sync" — so the message is misleading.

- [x] **Step 1: Rename the key and update the text**

In `Translate.elm`, rename the `IdleWaitingForSync` constructor to `PhotosTransferIdle` — relocating it alphabetically in both the `Translate` union type and the translation `case` (it moves from the `I` group to just before `PhotosTransferStatus`) — and set its `english` text to `"Idle — no photos pending"` (other languages stay `Nothing`). In `Pages/Device/View.elm`, `viewPhotosTransferInfo`'s `DownloadPhotosIdle` branch now uses `Translate.PhotosTransferIdle`.

- [x] **Step 2: Verify**

`grep -rn IdleWaitingForSync src/` returns nothing; `elm-format --validate` on the two files is clean; `elm make src/elm/Main.elm --output=/dev/null` compiles (exit 0).

- [x] **Step 3: Commit**

```bash
git add client/src/elm/Translate.elm client/src/elm/Pages/Device/View.elm
git commit -m "Fix misleading idle message in photos transfer status" # + body, Issue #1743, [ci skip]
```

---

## Task 3: Full baseline lint + test verification

**Status: DONE.** elm-format clean for the touched files (the only files `elm-format --validate client/src/` flags are pre-existing generated/config noise — `src/generated/**`, `Config.Deploy.elm` — untouched by this branch). `elm-review` clean. `elm-test` 3/3. Full `elm make` compiles.

**Files:** none modified (verification only; commit only if `elm-format` reformats something).

### Context

Tasks 1 and 2 ran targeted `elm-format` and test checks. This task runs the full baseline the spec calls for, to catch anything the targeted checks missed.

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

Expected: `I found no errors!`. Note: `SchedulePhotosDownload` and `TryDownloadingPhotos` are intentionally retained and still referenced, so there is no unused-constructor concern. Local-env caveat: on a dev machine with a gitignored `Config.Deploy.elm` and/or a stale `src/generated/generated.bak/` directory present, `elm-review` aborts with a "several modules named X" global error — CI never sees those files. To run elm-review locally, temporarily move those artifacts aside and restore them afterwards.

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

## Task 4: Manual throttled-network verification

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

- **Spec coverage:** Task 1 covers the guard removal, Task 2 the deferred-photos kick, and Task 2b the idle status-message fix (spec §The changes). Task 3 covers the automated baseline; Task 4 the manual throttled-network test, the load-bearing verification (spec §Testing). Branching is done — branch `parallel-photo-download` off `bulk-photo-fetch`, spec + plan committed; PR #1744 open.
- **Post-sync kick kept by design:** an earlier draft removed `SchedulePhotosDownload` / `TryDownloadingPhotos`; that was dropped on review — the kick is the tested mechanism that starts photos after a fast small-HC sync, and `TryDownloadingPhotos` already no-ops when the photo lane is in process. See "Design note" above. `TryDownloadingPhotos` also gains a second caller in Task 2 (the deferred-photos save handler).
- **No Model.elm field change:** the spec's earliest draft proposed splitting `downloadRequestTime`; investigation confirmed the photo lane never touches that field (it tracks in-flight state via `backendRemoteData` inside `downloadPhotosStatus`), so no field split is needed. The spec was corrected accordingly.
- **Issue linking:** the issue for this work is **#1743** (used by the Task 2 commit and linked from PR #1744). The earlier guard-removal commits reference `#1741` — a placeholder carried from the `bulk-photo-fetch` branch context before #1743 existed. Per the repo convention, issues are linked with `Issue #N.`, never `Closes`/`Fixes`.
