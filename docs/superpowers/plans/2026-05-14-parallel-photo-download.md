# Parallel data + photo download — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the SyncManager photo-download lane run concurrently with the data-download lane, so deferred photos start downloading from the first entity batches instead of waiting for the entire ~500K-entity sync to settle.

**Architecture:** The `SyncManager` already has two independent timer-driven lanes (`BackendFetchMain` for data, `BackendFetchPhotos` for photos). A single guard in `determineDownloadPhotosStatus` pins the photo lane to idle whenever the data lane is active. This plan removes that guard, then removes the now-redundant `SchedulePhotosDownload` / `TryDownloadingPhotos` debounce machinery. The two lanes share no mutable model state, so no other changes are needed.

**Tech Stack:** Elm 0.19.1. Tests via `elm-test` (run per-file: `npx elm-test <path>` — `npx` so the project-pinned `elm-test@0.19.1-revision6` is used, not a newer global one). Linting via `elm-format` and `elm-review`.

**Spec:** `docs/superpowers/specs/2026-05-14-parallel-photo-download-design.md`

**Branch:** `parallel-photo-download` (already created off `bulk-photo-fetch`; the spec doc is already committed on it).

---

## File Structure

- `client/src/elm/SyncManager/Utils.elm` — **modify.** `determineDownloadPhotosStatus` loses the `case model.syncStatus of …` guard wrapper.
- `client/src/elm/SyncManager/Test.elm` — **create.** First unit test for `SyncManager`; pins the guard-removal behaviour. Follows the repo's co-located `*/Test.elm` pattern (e.g. `App/Test.elm`).
- `client/src/elm/SyncManager/Model.elm` — **modify.** Remove the `SchedulePhotosDownload` and `TryDownloadingPhotos` constructors from the `Msg` type.
- `client/src/elm/SyncManager/Update.elm` — **modify.** Remove the `SchedulePhotosDownload` handler, the `TryDownloadingPhotos` handler, and the two `SchedulePhotosDownload` dispatch sites.

---

## Task 1: Remove the sync-active guard in `determineDownloadPhotosStatus`

**Files:**
- Create: `client/src/elm/SyncManager/Test.elm`
- Modify: `client/src/elm/SyncManager/Utils.elm` (function `determineDownloadPhotosStatus`)
- Test: `client/src/elm/SyncManager/Test.elm`

### Context

`determineDownloadPhotosStatus` currently wraps its photo state-machine logic in `case model.syncStatus of SyncIdle -> <inner> ; _ -> DownloadPhotosIdle`. The `_ -> DownloadPhotosIdle` branch is the guard: while the data lane is doing anything (`syncStatus /= SyncIdle`), the photo lane is forced idle. Removing the wrapper makes the inner logic run unconditionally. The outer `syncCycleRotate` check (is sync enabled at all) stays.

- [ ] **Step 1: Write the failing test**

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

- [ ] **Step 2: Run the test to verify it fails**

Run from `client/`:

```bash
cd client && npx elm-test src/elm/SyncManager/Test.elm
```

Expected: the **first** test FAILS — `Expect.notEqual DownloadPhotosIdle` fails because the guard pins `downloadPhotosStatus` to `DownloadPhotosIdle` when `syncStatus = SyncDownloadAuthority …`. The **second** test PASSES (it is a regression guard for the `SyncCyclePause` behaviour, which this change does not touch).

If `elm-test` errors before running (compilation/dependency error rather than a test failure), `rm -rf client/elm-stuff` and re-run.

- [ ] **Step 3: Remove the guard**

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

- [ ] **Step 4: Run the test to verify it passes**

Run from `client/`:

```bash
cd client && npx elm-test src/elm/SyncManager/Test.elm
```

Expected: BOTH tests PASS.

- [ ] **Step 5: Run elm-format on the touched files**

Run from the repo root:

```bash
elm-format --validate client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm
```

Expected: no output (both files are correctly formatted). If it reports a diff, run `elm-format client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm` to auto-format, then re-run `npx elm-test src/elm/SyncManager/Test.elm` to confirm tests still pass.

- [ ] **Step 6: Commit**

```bash
git add client/src/elm/SyncManager/Utils.elm client/src/elm/SyncManager/Test.elm
git commit -m "$(cat <<'EOF'
Remove sync-active guard so photo lane runs in parallel

determineDownloadPhotosStatus pinned the photo-download lane to idle
whenever the data lane was active, so on a large-HC initial sync photos
only started after the entire entity sync settled. Removing the guard
lets the two existing timer-driven lanes run concurrently; they share no
mutable model state. Adds the first SyncManager unit test.

Issue #1741.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>

[ci skip]
EOF
)"
```

---

## Task 2: Remove the redundant `SchedulePhotosDownload` / `TryDownloadingPhotos` debounce machinery

**Files:**
- Modify: `client/src/elm/SyncManager/Update.elm` (handlers at the `SchedulePhotosDownload` and `TryDownloadingPhotos` `case` branches; two dispatch sites)
- Modify: `client/src/elm/SyncManager/Model.elm` (`Msg` type constructors)

### Context

`SchedulePhotosDownload` debounced a `TryDownloadingPhotos` message to kick photo download ~15s after a sync cycle settled. With the guard removed in Task 1, the photo lane runs continuously off its own timer, so this kick is redundant. `TryDownloadingPhotos` is dispatched *only* from the `SchedulePhotosDownload` handler, so it is orphaned and must be removed too. Elm compiles fine with an unused `Msg` constructor, but `elm-review`'s `NoUnused.CustomTypeConstructors` rule flags it — hence both go in this task.

`BackendFetchPhotos` (which the `TryDownloadingPhotos` handler called) stays — it is still dispatched by the photo-lane `Time.every` subscription. `SchedulePageRefresh`, `RefreshPage`, and the `debouncer` model field all stay (the debouncer is still used by `SchedulePageRefresh`).

This task is a compiler- and lint-verified refactor: there is no behaviour to assert in a unit test (it removes messages, it does not change a pure function), so verification is "the project compiles and `elm-review` is clean".

- [ ] **Step 1: Remove the first `SchedulePhotosDownload` dispatch site**

In `client/src/elm/SyncManager/Update.elm`, inside the `BackendAuthorityFetch` handler's `Nothing ->` branch, find:

```elm
                                determineSyncStatus
                                    |> sequenceSubModelReturn (update currentTime activePage dbVersion device)
                                        [ SchedulePhotosDownload, QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]
```

Replace with:

```elm
                                determineSyncStatus
                                    |> sequenceSubModelReturn (update currentTime activePage dbVersion device)
                                        [ QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]
```

- [ ] **Step 2: Remove the second `SchedulePhotosDownload` dispatch site**

In `client/src/elm/SyncManager/Update.elm`, inside the `BackendAuthorityDashboardStatsFetchHandle` handler's `extraMsgs` computation, find:

```elm
                        else
                            [ SchedulePhotosDownload, QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]
```

Replace with:

```elm
                        else
                            [ QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]
```

- [ ] **Step 3: Remove the `SchedulePhotosDownload` handler**

In `client/src/elm/SyncManager/Update.elm`, find and delete this `case` branch (including the blank line that follows it, so `RefreshPage ->` ends up directly after `SchedulePageRefresh`'s branch):

```elm
        SchedulePhotosDownload ->
            noChange
                |> sequenceSubModelReturn (update currentTime activePage dbVersion device)
                    [ MsgDebouncer <| provideInput TryDownloadingPhotos ]

```

- [ ] **Step 4: Remove the `TryDownloadingPhotos` handler**

In `client/src/elm/SyncManager/Update.elm`, find and delete this `case` branch — it is the last branch of the `case msg of` block, immediately before the `subscriptions` function. Delete from the blank line before `TryDownloadingPhotos ->` through its final `noChange`:

```elm

        TryDownloadingPhotos ->
            case model.downloadPhotosStatus of
                DownloadPhotosIdle ->
                    update
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendFetchPhotos
                        model

                _ ->
                    -- Sync is already in progress.
                    noChange
```

After deletion, the line `noChange` ending the preceding handler is followed by two blank lines and then `subscriptions : Model -> Sub Msg`.

- [ ] **Step 5: Remove the `Msg` constructors**

In `client/src/elm/SyncManager/Model.elm`, in the `type Msg` declaration, delete this line:

```elm
    | SchedulePhotosDownload
```

and, further down in the same `type Msg` declaration, delete this line:

```elm
    | TryDownloadingPhotos
```

- [ ] **Step 6: Compile to verify no dangling references**

Run from `client/`:

```bash
cd client && elm make src/elm/Main.elm --output=/dev/null
```

Expected: `Success!` — compilation completes with no errors. A "naming error" or "unknown variable" here means a `SchedulePhotosDownload` / `TryDownloadingPhotos` reference was missed.

- [ ] **Step 7: Run elm-format on the touched files**

Run from the repo root:

```bash
elm-format --validate client/src/elm/SyncManager/Update.elm client/src/elm/SyncManager/Model.elm
```

Expected: no output. If it reports a diff, run `elm-format client/src/elm/SyncManager/Update.elm client/src/elm/SyncManager/Model.elm` to auto-format, then re-run Step 6 to confirm it still compiles.

- [ ] **Step 8: Commit**

```bash
git add client/src/elm/SyncManager/Update.elm client/src/elm/SyncManager/Model.elm
git commit -m "$(cat <<'EOF'
Remove redundant SchedulePhotosDownload debounce machinery

With the photo lane running continuously off its own timer, the
debounced kick that scheduled photo download after a sync cycle settled
is redundant. TryDownloadingPhotos was dispatched only by the
SchedulePhotosDownload handler, so it is removed as well. BackendFetchPhotos
stays, dispatched by the photo-lane timer subscription.

Issue #1741.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>

[ci skip]
EOF
)"
```

---

## Task 3: Full baseline lint + test verification

**Files:** none modified (verification only; commit only if `elm-format` reformats something).

### Context

Tasks 1 and 2 ran targeted `elm-format` and compile checks. This task runs the full baseline the spec calls for, to catch anything the targeted checks missed (notably `elm-review`'s unused-constructor rule confirming `SchedulePhotosDownload` / `TryDownloadingPhotos` are fully gone).

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

Expected: `I found no errors!` — in particular, no `NoUnused.CustomTypeConstructors` error for `SchedulePhotosDownload` or `TryDownloadingPhotos`. If either is still flagged, a reference or the constructor itself was missed in Task 2 — fix it, then re-run.

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

- [ ] **Step 4: Confirm the five spec checks**

While the initial sync runs, confirm each of these. Record the result for each:

1. The data lane progresses and the sync eventually completes — `syncStatus` reaches `SyncIdle`, the sync does not hang.
2. Bulk-photo requests (`POST` to the bulk-photos endpoint) appear in the Network tab **interleaved** with the `/api/sync` requests, starting early in the entity download — not only after the entity download finishes.
3. Neither lane hangs or stalls.
4. The `>45s`-sync page refresh still fires (the app reloads itself after the long sync settles).
5. After that refresh, the photo lane resumes — bulk-photo requests continue draining `deferredPhotos` post-reload.

- [ ] **Step 5: Record the outcome**

If all five checks pass, the feature is verified — note this in the PR description. If any check fails, do **not** proceed to merge; capture the failure (which check, what happened, screenshots of the Network tab) and report back — this is the slow-device regression the spec warned about, and it needs investigation before this ships.

---

## Self-Review Notes

- **Spec coverage:** Task 1 covers "Remove the guard" (spec §The changes #1). Task 2 covers "Remove `SchedulePhotosDownload` and `TryDownloadingPhotos`" (spec §The changes #2). Task 3 covers the automated baseline (spec §Testing). Task 4 covers the manual throttled-network test (spec §Testing, the load-bearing verification). Branching is already done (spec §Branching and integration) — branch `parallel-photo-download` exists off `bulk-photo-fetch` with the spec committed.
- **No Model.elm field change:** the spec's earlier draft proposed splitting `downloadRequestTime`; investigation during planning confirmed the photo lane never touches that field (it tracks in-flight state via `backendRemoteData` inside `downloadPhotosStatus`), so no field split is needed. The spec was corrected accordingly.
- **`Issue #1741`** is used in commit messages per the repo convention of linking issues without `Closes`/`Fixes`. Confirm 1741 is the correct issue for this work before committing; if a different issue tracks the parallel-download work, substitute it.
