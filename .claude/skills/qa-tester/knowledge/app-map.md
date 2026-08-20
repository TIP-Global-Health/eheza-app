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
