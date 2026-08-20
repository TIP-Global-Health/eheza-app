# QA App Map — E-Heza

Stable facts about navigating and preparing the app for manual QA. Correct this file whenever
reality disagrees with it; add facts you had to discover the hard way.

For test accounts, page/activity selectors, and per-encounter-type mechanics, use
`../../e2e-test/references/e2e-knowledge-base.md` — that file is the source of truth for those.

## Environment

- App URL: `http://localhost:3000` (served by `ddev gulp` from the **main tree**
  `/var/www/html/ihangane` — it serves whatever branch that tree is on).
- After every Elm recompile: click **"Version"** in the app's top-right corner to activate the
  new code (service-worker update). Skipping this means testing the previous build.
- `EHEZA_SITE` env var selects rwanda/burundi (set in `.ddev/config.local.yaml`).
- Backend admin: Drupal at the ddev URL, `admin`/`admin`.
- Default device pairing code: `12345678` (single-use; e2e infrastructure uses `99999999`).
  Re-pairing an already-paired device needs super user mode:
  `ddev drush vset hedley_super_user_mode 1` (set back to 0 after).
- Default nurse PIN: `1234`. Other accounts: see the e2e knowledge base accounts table.

## Feature flags

`ddev drush vset hedley_admin_feature_<name>_enabled 1|0` — flags: `ncda`, `stock_management`,
`tuberculosis_management`, `group_education`, `report_to_whatsapp`, `hiv_management`,
`gps_coordinates`, `family_nutrition`. A screen behind a disabled flag simply does not appear —
check the flag before concluding a screen is unreachable.

## Sync

Offline-first: the client writes to browser storage and syncs in the background. Backend
effects (nodes created, reports updated) are visible only after the upload lane drains. Verify
backend state with `ddev drush sqlq ...` / `ddev drush` queries.

## Navigation facts

(Append route recipes here as runs discover them: screen → click path from login.)

- Manual QA device (since 2026-08-20): node "QA Manual Device 2124". Its pairing code
  77777777 is consumed (codes are single-use). To re-pair, create a fresh device node with a
  new code via drush (PHP in `client/e2e/helpers/device.ts`, `resetDevice`). The previous
  "Manual Browser Device" was abandoned with a poisoned upload queue (see pitfalls).
- e2e person titles are `E2ETest <firstName>` (registerAdult hardcodes the second name and
  Drupal stores "secondName firstName").

## Sync (facts discovered 2026-08-20)

- Sync network requests go through the service worker and do not show in the tab's request
  log; judge sync by Device Status timestamps and the local `sync` IndexedDB instead.
- Sync Manager debug panels live behind the top-left "Error log" / "Sync Manager" links on
  the Device Status page; Sync Settings exposes Idle time (default 600000 ms — after an
  error the lane sleeps 10 minutes; lower it to retry quickly).
- After a wiped device pairs, general data (nurses) must download before any PIN works —
  "Your PIN code was not recognized" right after pairing just means sync hasn't finished.

## Navigation facts

- Register + first encounter (nurse): Clinical → Individual Encounter → <type> → search →
  Register a new participant → form (names, DOB via calendar popup, gender, education,
  marital status, NCD also Mode of delivery, address cascade Province→…→Village, Health
  Center) → Save → encounter-type select page → First/Subsequent encounter.
- Prenatal Laboratory point-of-care blood sugar: Laboratory activity → "Random Blood Sugar"
  tab (right end of the tab strip) → performed today Yes → Point of Care → before meal → the
  mg/dL input appears.
- NCD Laboratory opens directly on Random Blood Sugar; extra question "Did you perform this
  test today?" before the input appears.
- Labs history: subsequent prenatal encounter → Laboratory shows ONLY the History task while
  labs from previous encounters are pending → each row's UPDATE opens
  `#prenatal-labs-history/...` with that test's result form.
- Same-day second encounter is impossible; backdate the first via drush
  (`field_scheduled_date` value+value2, see e2e `backdateEncounter`) — no client resync
  needed when the client re-downloads afterwards (fresh device) or syncs before starting.
- Case Management (nurse): All / Contact Tracing / ANC Labs / NCD Labs panes; the forward
  icon on an ANC/NCD Labs entry opens the recurrent Lab Results page directly. Lab tech
  (PIN 3333) sees only Case Management + Device Status, ANC Labs pane only.
