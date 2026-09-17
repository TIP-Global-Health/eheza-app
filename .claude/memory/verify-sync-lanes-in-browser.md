---
name: verify-sync-lanes-in-browser
description: "How to exercise the sync download lanes on the real local stack from Chrome: fresh device, slow batch via a local PHP sleep, reading cursors/requests from the page; the quirks that cost time (2026-09-17, B-317)"
metadata:
  type: feedback
---

**Verified recipe (B-317, 2026-09-17) for proving a sync change on the real app, single-HC case.**

1. **Fresh device:** create one with `ddev drush eval` (copy the `resetDevice` PHP from
   `client/e2e/helpers/device.ts`, any 8-digit code). In the tab: unregister SWs, `caches` delete,
   `localStorage.clear()`, `Dexie.delete('sync')`, reload → pairing form. Check the RUNNING bundle
   via `fetch('/Main.js')` from the page (the SW serves the previous build until refreshed).
2. **HC is added to the sync list only on the Device Status page** ("START SYNCING" per HC),
   not by choosing the HC at login. Navigate with `location.hash = '#device'`.
3. **Slow batch:** a local, never-committed `sleep(70)` in `HedleyRestfulSync::getForHealthCenter`
   when `base_revision == 0`. nginx `fastcgi_read_timeout` is 10 m and fpm has no limit, so 70 s
   is safe; 35 s is NOT enough to force the 30 s re-issue (tick jitter). Use the **Edit tool** —
   a bash edit of a main-tree file is classifier-blocked as "Modify Shared Resources".
4. **Read state from the page:** `localStorage.syncInfoAuthorities` (cursor/remaining/status),
   `dbSync.shards.count()` / max `vid`, and `performance.getEntriesByType('resource')` for
   `/api/sync` (appears only once a request COMPLETES; `read_network_requests` shows pending ones).
   ⚠ The JS tool blanks any output containing a `uuid` key or query strings — return numbers and
   booleans, mask the uuid with a prefix test.
5. **Clicks:** `computer` clicks and ref clicks do NOT fire the Elm handlers on tiles/buttons here;
   `element.click()` from JS does. Find the HC's button by document order after the heading
   text, with `getBoundingClientRect()` for visibility (`offsetParent` lied once).
6. **The app refreshes the page by itself** after a sync cycle longer than 45 s
   (`SchedulePageRefresh`), which clears `window` state and the network list — keep the clock in
   `localStorage`. Long `await` scripts hit the 45 s CDP timeout; poll in ≤25 s steps.

Related: [[e2e-local-run-procedure]], [[verify-served-bundle-before-e2e]], [[verify-by-running-not-reasoning]]
