# Parallel data + photo download — design

**Date:** 2026-05-14
**Branch:** `parallel-photo-download` (off `bulk-photo-fetch`)

## Background

E-Heza's `SyncManager` runs two timer-driven lanes:

- **Data lane** — a `Time.every` subscription fires `BackendFetchMain`. It is
  driven by `model.syncStatus` and advanced by
  `SyncManager.Utils.determineSyncStatus`. It uploads and downloads entity
  metadata in batches.
- **Photo lane** — a separate `Time.every` subscription fires
  `BackendFetchPhotos`. It is driven by `model.downloadPhotosStatus` and
  advanced by `SyncManager.Utils.determineDownloadPhotosStatus`. It drains the
  `deferredPhotos` IndexedDB table (now via the bulk-photo fetch path added on
  the `bulk-photo-fetch` branch).

The two lanes already have independent subscriptions and independent speed /
back-off functions (`getSyncSpeedForSubscriptions` and
`getDownloadPhotosSpeedForSubscriptions`). They are *not*, however, allowed to
run at the same time.

## Problem

`determineDownloadPhotosStatus` contains an explicit guard
(`SyncManager/Utils.elm`, the `case model.syncStatus of SyncIdle -> … ; _ ->
DownloadPhotosIdle` wrapper): whenever `syncStatus /= SyncIdle`, the photo lane
is pinned to `DownloadPhotosIdle`. The `BackendFetchPhotos` timer keeps firing,
but the photo state machine never progresses while the data lane is busy.

Consequence: for a large health center — an initial sync can be ~500K entities,
many batches of up to 500 — the data lane keeps `syncStatus` non-idle for a
long time, so photo download does not begin until the entire entity sync has
settled. Photos that were already referenced in `deferredPhotos` from the very
first batches sit untouched until the end.

### History

This guard was added deliberately. `git blame` points to commit `ec00fd6b1`
("Avoid parallel download of data and photos"), part of PR #2057 "Resolve
initial sync problems on slow devices" (branch `Gizra/issue-2056`, merged
2020-12-04). The companion commit `42793565e` removed an inline
`TryDownloadingPhotos` that previously ran after every authority fetch batch,
and replaced it with a 15-second debounced `SchedulePhotosDownload`. In other
words: cooperative interleaving of data and photo download *was* the original
behaviour and was removed because it caused initial-sync problems on slow
devices.

The original GitHub issue/PR bodies are not recoverable — the repository
migrated from `Gizra/eheza-app` to `TIP-Global-Health/eheza-app` and the old
issue numbers do not resolve on the current repo. The commit messages do not
record the precise failure mode. This is the known risk this change re-opens,
and the reason verification (below) is explicit.

Note also that the 2020 decision predates the `bulk-photo-fetch` work. At that
time photo download was one-at-a-time; the contention profile of parallel
download is materially different now that photos are fetched in bulk batches.

## Goal

Let the photo lane run concurrently with the data lane, so photo download
begins as soon as `deferredPhotos` has rows — from the first entity batches —
instead of waiting for the whole entity sync to finish.

## Non-goals

- **Throttling or prioritising the photo lane.** The two lanes run full-tilt
  and compete equally for bandwidth and the JS main thread. A data-prioritised
  or adaptive lane was considered and rejected in favour of the simplest change
  that delivers the functional win. See "Rejected alternatives".
- **Extracting the photo lane into its own module.** Better structural
  isolation, but a much larger diff touching `App/Model`, `App/Update`, and
  ports wiring — exactly the kind of broad change that makes a slow-device
  regression hard to bisect. Can be done later as a focused task if needed.
- **Changes to the bulk-photo fetch mechanism itself**, the upload-photo path,
  or entity metadata sync.
- **Removing the post-long-sync page refresh.** Kept as-is (see below).
- **Removing the post-sync photo-download kick.** `SchedulePhotosDownload` /
  `TryDownloadingPhotos` are intentionally kept — see "The changes".

## Approach

Make the photo lane run concurrently with the data lane by removing the single
guard that couples them. No new machinery — the two timers, the photo state
machine, and the photo speed/back-off function all already exist and work. We
are un-gating proven code.

The two lanes turn out to share **no mutable model state**. Investigation
confirmed `downloadRequestTime` (the timeout clock) is used only by the data
lane — `BackendAuthorityFetch` and `BackendGeneralFetch`. The photo lane tracks
its in-flight request via `backendRemoteData` inside the `downloadPhotosStatus`
record, which is entirely separate. Elm's runtime also processes `update` calls
one at a time, so the only contention is at runtime (bandwidth + main thread),
not in the model. No field split is needed.

### Rejected alternatives

- **Equal full-tilt vs. data-prioritised vs. adaptive.** Full-tilt was chosen.
  On a slow link, running both lanes uncapped roughly doubles the data sync's
  wall-clock time (bandwidth shared ~50/50, plus main-thread contention
  decoding responses). The accepted trade-off: the functional win — photos
  starting at batch 1 instead of after ~500K entities — is captured fully by
  the simplest possible change, and a throttling policy can be layered on later
  if field measurement shows data sync is hurt too much.
- **Extract photo lane into its own sub-module.** Rejected as contrary to the
  "simplest change" goal and higher-risk for the slow-device regression.

## The changes

Two changes, in two files. The post-sync photo-download kick is intentionally
kept (see below).

### 1. Remove the guard — `SyncManager/Utils.elm`

In `determineDownloadPhotosStatus`, remove the `case model.syncStatus of
SyncIdle -> <inner> ; _ -> DownloadPhotosIdle` wrapper. The inner photo
state-machine logic runs unconditionally, so the photo lane progresses
regardless of what the data lane is doing.

The outer `syncCycleRotate` check (`model.syncCycle == SyncCycleOn` — "is sync
enabled at all") is untouched. When sync is paused, both lanes still pause.

### 2. Kick the photo lane when deferred-photo rows land — `SyncManager/Update.elm`

Removing the guard lets the photo lane *progress* during sync, but it does not
*bootstrap* it: the lane leaves `DownloadPhotosIdle` only on a
`BackendFetchPhotos` timer tick, and that timer's idle interval comes from
`getDownloadPhotosSpeedForSubscriptions`, which returns `syncSpeed.idle`
verbatim. `syncSpeed.idle` defaults to **10 minutes** (`client/src/js/app.js`,
`getSyncSpeed`). So with only the guard removed, photos still took *minutes* to
start — the lane sat idle waiting out that 10-minute tick. (Confirmed by
testing: photos did start *during* sync, but minutes late.)

The fix: in the `SavedAtIndexDbHandle` handler, add a case for
`IndexDbSaveResultTableDeferredPhotos` that dispatches `TryDownloadingPhotos`.
When the data lane writes deferred-photo rows, the JS side confirms the save
via the `savedAtIndexedDb` port; that confirmation now kicks the photo lane out
of `DownloadPhotosIdle` immediately — race-free (the rows have already landed),
and through a single point that covers every deferred-photo write path.

Cold-start latency drops from up to ~10 minutes to roughly one fast
(`cycle`-interval) timer tick plus an IndexedDB round-trip.

### The post-sync kick is kept, not removed

`SchedulePhotosDownload` debounces a `TryDownloadingPhotos` message to kick
photo download ~15s after a sync cycle settles. An earlier draft of this design
removed it as "redundant" once the photo lane runs continuously. That removal
was dropped:

- **Small HCs are the common case.** Their sync settles fast, and the post-sync
  kick is the well-tested mechanism that gets photos going. Keeping it
  preserves the proven behaviour for the common path.
- **The "already running" concern is already handled.** The worry with keeping
  the kick is that it could fire while the photo lane is *already running* (now
  possible, since the lanes run in parallel). The existing `TryDownloadingPhotos`
  handler already guards this:

  ```elm
        TryDownloadingPhotos ->
            case model.downloadPhotosStatus of
                DownloadPhotosIdle ->
                    update ... BackendFetchPhotos model

                _ ->
                    -- Sync is already in progress.
                    noChange
  ```

  If the photo lane is in process (`downloadPhotosStatus` is anything but
  `DownloadPhotosIdle`), the kick is a clean `noChange` no-op — no double-start,
  no breakage.

So `SchedulePhotosDownload`, the `debouncer` model field, and the post-sync
dispatch sites are all left untouched. `TryDownloadingPhotos` is also kept — it
simply gains a second caller (the deferred-photos save handler in change 2
above).

## Architecture and data flow

After the change, the two lanes are fully independent — each has its own timer,
its own state field (`syncStatus` / `downloadPhotosStatus`), its own in-flight
tracking (the data lane's `downloadRequestTime` timeout clock; the photo lane's
`backendRemoteData` inside `downloadPhotosStatus`), and its own speed/back-off
function. They never coordinate and share no mutable model state. The
`deferredPhotos` IndexedDB table remains the implicit producer→consumer queue:
the data lane writes photo-URL rows as entity batches land, the photo lane
drains them.

On a large-HC initial sync:

```
Data lane:   [batch 1][batch 2][batch 3] ... [batch N]  → SyncIdle
                  │        │        │
                  ▼        ▼        ▼   (photo-URL rows written to deferredPhotos)
Photo lane:     [bulk fetch][bulk fetch][bulk fetch] ...  (runs concurrently,
                                                           from batch ~1 onward)
```

- The photo lane is kicked out of idle the moment deferred-photo rows are saved
  (change 2), so "from batch ~1 onward" is real — it does not wait out the
  photo lane's slow (~10-minute) idle timer.
- Photo download also now runs during the *upload* phases of a cycle
  (`SyncUploadGeneral`, `SyncUploadAuthority`, etc.), not just downloads — a
  direct and intended consequence of removing the guard.
- When the authority sync settles and took **>45s**, `SchedulePageRefresh`
  still fires. The resulting reload restarts the Elm app and interrupts any
  in-flight photo fetch; this is recoverable — the photo lane resumes draining
  `deferredPhotos` after restart, since that table is persisted.

## Error handling

- Each lane already has independent network-error back-off:
  `getSyncSpeedForSubscriptions` and `getDownloadPhotosSpeedForSubscriptions`
  both slow their own timer on a `Failure`. Concurrent failures in the two
  lanes do not interfere.
- In-flight tracking is already per-lane: the data lane uses
  `downloadRequestTime` (a timeout clock); the photo lane uses
  `backendRemoteData` inside `downloadPhotosStatus`. Neither touches the other.
- There is no shared mutable model state between the two lanes, so removing the
  guard introduces no data-level race. Elm processes `update` calls one at a
  time; the only contention is at runtime (bandwidth + main thread), which is
  the accepted full-tilt trade-off.

## Branching and integration

The implementation branch `parallel-photo-download` is cut from
`bulk-photo-fetch`, not `develop`:

- The parallel-download win depends on photo fetching being efficient (bulk
  batches). On `develop`, photo download is still one-at-a-time —
  parallelising that against data sync would be lower-value and would have a
  different, worse contention profile than what this design analysed.
- Conceptually this is the sequel to the bulk-photo-fetch work, so it stacks on
  top of it and follows it downstream.

The design doc and all implementation commits land on
`parallel-photo-download`. It merges back into `bulk-photo-fetch`, or follows
it into `develop` once that merges.

## Testing

**Baseline (automated):**

- `elm-format --validate client/src/`
- `elm-review` from `client/` — run `rm -rf client/elm-stuff` first (known
  stale-cache caveat).
- `elm-test` from `client/`.

**Manual throttled-network test (the load-bearing verification):**

The 2020 regression was "initial sync problems on slow devices", so the
verification deliberately reproduces those conditions. Using Chrome DevTools
network throttling set to **Slow 3G**, against a large health center, confirm:

1. The data lane still progresses and `syncStatus` reaches `SyncIdle` — the
   sync completes, it does not hang.
2. Bulk-photo requests appear in the Network tab **interleaved** with sync
   requests, starting *promptly* (within seconds of the first entity batch that
   carries photos) — not minutes late, and not only after the entity download
   finishes.
3. No hang or stall in either lane.
4. The `>45s`-sync page refresh still fires.
5. The photo lane resumes draining `deferredPhotos` after that refresh.
6. On a *small* HC (fast sync), the post-sync `SchedulePhotosDownload` kick
   firing while the photo lane may already be running causes no double-fetch,
   error, or stall — it is absorbed cleanly by `TryDownloadingPhotos`' guard.

## Things to verify during implementation

1. Confirm `determineDownloadPhotosStatus` has no implicit dependency on
   `syncStatus` beyond the guard being removed.
2. Confirm the post-sync kick is a safe no-op when the photo lane is already
   running — `TryDownloadingPhotos` should `noChange` whenever
   `downloadPhotosStatus /= DownloadPhotosIdle`. Verified by code inspection and
   exercised in the manual throttled-network test on a small HC.
3. Confirm the photo lane running during data-lane *upload* phases does not
   collide with the photo *upload* path — they use different IndexedDB tables,
   but verify under the manual test.

## Future work

- A throttling / prioritisation policy for the photo lane if field measurement
  shows the data sync is hurt too much by full-tilt contention.
- Extract the photo lane into its own module for structural isolation, if it
  needs independent testing or further evolution.
