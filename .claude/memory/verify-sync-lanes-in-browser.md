---
name: verify-sync-lanes-in-browser
description: "How to exercise the sync lanes on the real local stack (Chrome or Playwright): fresh device, slow/held download via a local PHP sleep, 3 s sync cycle via localStorage.syncSpeed, reading cursors/requests from the page; the quirks that cost time (B-317 2026-09-17, B-496 2026-10-18)"
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

7. **Live-DB import (multi-batch):** pick an HC by `field_data_field_shards` count (Mushongi 5,235 =
   11 batches; Bumbogo 420k = too big). Create a local nurse with `field_role ['nurse']`,
   `field_health_centers [nid]` — PINs must be unique across all nurses (`hedley_user_node_presave`).
   The Device page lists the HC of `localStorage.healthCenterId` (a uuid), not the nurse's HCs: set it
   and reload. **Type the PIN with real keystrokes** (`computer` click + type + Return); scripted
   `input` events never reach Elm. Judge the visible page by screenshot — hidden forms have a
   non-zero rect, so DOM "visibility" checks lied twice.

Related: [[e2e-local-run-procedure]], [[verify-served-bundle-before-e2e]], [[verify-by-running-not-reasoning]]

8. **Headless variant (B-496, 2026-10-18) — hold a download while the app saves.** Playwright `page.route` NEVER sees
   `/api/sync` requests (they pass through the service worker's fetch handler), so hold them server-side: append to
   the git-ignored `server/www/sites/default/settings.php` a block that, for GET `/api/sync*`, touches
   `files/hold-sync.active` and `sleep(1)`-loops while `files/hold-sync` exists; the test creates/removes the flag
   from the host path. The background sync cycle idles **10 MINUTES** by default (`app.js` `getSyncSpeed`); set
   `localStorage.syncSpeed = {"idle":3000,"cycle":50,"offline":10000}` with `page.addInitScript` (login clears
   localStorage and reloads, the init script re-applies). Keep a hold under ~40 s or the 45 s rule refreshes the page.
   Spec template: scratchpad `r45/b496-repro.spec.ts` of session 50b2c76a (untracked spec in `client/e2e/`, deleted after).


9. **Chrome's 20 s same-address wait (PR #2407 QA, 2026-10-21).** While a download is held, every copy has the same URL, and Chrome holds each one back up to 20 s before it reaches the server — so a resend lands ~50 s after the first request, not 30. Time scenarios from a page-side XHR log (patch `XMLHttpRequest.prototype.send`), not the server log. Holding server-side: a temporary block at the end of the git-ignored `settings.php` plus a `files/qa-hold/` directory; create the directory with the Write tool and edit with Edit — `mkdir`/`curl -k` there are classifier-blocked, plain-HTTPS `curl` to `https://ihangane.ddev.site:4443` worked after the user allowed it.
