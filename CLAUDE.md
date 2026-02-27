# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

E-Heza is a digital health data capture app for frontline health workers (maternal/child health). It's an offline-first PWA that syncs with a Drupal backend.

- **Frontend:** Elm 0.19.1 (`client/`)
- **Backend:** Drupal 7 with custom "Hedley" install profile (`server/hedley/`)
- **Database:** MariaDB 10.5
- **Dev environment:** DDEV (Docker-based)
- **Hosting:** Pantheon
- **Multi-site:** `EHEZA_SITE` env var switches between `rwanda` and `burundi`

## Build & Dev Commands

Start the environment with `ddev start`. All `ddev` commands run inside the container.

### Frontend

```bash
ddev gulp                # Serve on localhost:3000, watch for file changes
ddev gulp publish        # Minified production build
npm test                 # Runs: gulp zscore && gulp version && elm-test
```

After editing Elm code and it compiles, click "Version" in the app's top-right corner to activate the new version.

### Backend

```bash
cd server && ./install [-d] [-l] [-y]   # Install Drupal (-d=demo, -l=auto-login, -y=unattended)
drush cc all                             # Clear all caches
drush fra                                # Features revert all
drush updb                               # Run database updates
drush mi --group=default --user=1        # Run default data migrations
drush mi --group=counseling --user=1     # Run counseling migrations
drush mi --group=forms --user=1          # Run forms migrations
```

### Linting & Testing

```bash
# Elm
elm-format --validate client/src/        # Check formatting
elm-format client/src/                   # Auto-format
elm-test                                 # Run Elm unit tests (from client/)
elm-review                               # Elm linting (from client/)

# PHP
REVIEW_STANDARD="Drupal" ci-scripts/test_coder.sh
REVIEW_STANDARD="DrupalPractice" ci-scripts/test_coder.sh

# Drupal integration tests
ddev simpletest

# Shell scripts
ci-scripts/test_shell.sh

# E2E tests (Playwright, from client/)
./node_modules/.bin/playwright test              # Headless, fast
RECORD=1 ./node_modules/.bin/playwright test     # Headed, with video recording and visual cursor
# Video saved to client/test-results/*/video.webm
```

### Deployment

```bash
ddev auth ssh && ddev gulp publish && ddev robo deploy:pantheon
```

## Architecture

### Frontend (Elm) — `client/src/elm/`

Standard Elm **Model-Update-View** architecture. Entry point is `Main.elm` → `Browser.application`.

- `App/Model.elm`, `App/Update.elm`, `App/View.elm` — global state, routing, root view
- `Pages/` — one sub-module per health program (Prenatal, WellChild, NCD, HIV, Nutrition, AcuteIllness, Tuberculosis, etc.). Each typically has `Model.elm`, `Update.elm`, `View.elm`, and sub-pages for Activity/Encounter
- `Backend/` — REST API communication with the Drupal backend (41 sub-modules, one per entity type)
- `SyncManager/` — bidirectional offline sync logic
- `Measurement/` — health measurement types and calculations
- `ZScore/` — WHO Z-Score data and calculations
- `Translate.elm` — all UI translation strings (~1.3MB)
- `ServiceWorker/` — PWA/offline support (Workbox)
- `LocalConfig.elm` — local dev config (gitignored, copy from `LocalConfig.Example.elm`)
- Source directories: `src/elm/` and `src/generated/`

### Backend (Drupal 7) — `server/hedley/`

Custom installation profile with 33+ custom modules under `hedley/modules/custom/`:

- `hedley_restful` — 218+ REST API endpoint plugins (the API consumed by Elm)
- `hedley_person`, `hedley_device`, `hedley_health_center` — core entities
- `hedley_prenatal`, `hedley_well_child`, `hedley_nutrition`, `hedley_ncd`, `hedley_hiv`, `hedley_tuberculosis`, `hedley_acute_illness`, `hedley_family_nutrition` — health program modules
- `hedley_migrate` — CSV-based data migration (site-specific CSVs for Rwanda/Burundi)
- `hedley_admin` — admin UI and feature flag management
- `hedley_stats`, `hedley_reports` — statistics and reporting

Data is stored as Drupal nodes via the Field API. Key entity types: `device`, `nurse`, `health_center`, `person`, `encounter`, `activity`, `measurement`.

Configuration is managed as code via the **Features** module. After changing config in UI, export with `drush fu <feature_name>`.

### Data Sync

Offline-first: the Elm `SyncManager` handles bidirectional sync with the backend REST API. Data persists in browser storage offline; Drupal + MariaDB is the source of truth. Advanced Queue processes background tasks server-side.

### Feature Flags

Programs are toggled via `drush vset hedley_admin_feature_<name>_enabled 1|0`. Flags include: `ncda`, `stock_management`, `tuberculosis_management`, `group_education`, `report_to_whatsapp`, `hiv_management`, `gps_coordinates`, `family_nutrition`.

### Super User Mode

`ddev drush vset hedley_super_user_mode 1` — bypasses content unpublishing/deletion restrictions and allows resetting device pairing codes on already-paired devices. Disable with `ddev drush vset hedley_super_user_mode 0`.

## CI (CircleCI)

Four parallel jobs defined in `.circleci/config.yml`:
1. **lint_phpcs** — PHPCodeSniffer (Drupal + DrupalPractice standards)
2. **lint_elm** — `elm-format` validation
3. **lint_shellcheck** — ShellCheck on all shell scripts
4. **test_simpletest_linux** — full Drupal install + SimpleTest (runs after all lint jobs pass)

CI scripts live in `ci-scripts/`.

## Local Setup

```bash
cp .ddev/config.local.yaml.example .ddev/config.local.yaml   # set EHEZA_SITE, credentials
ddev restart
cp client/src/elm/LocalConfig.Example.elm client/src/elm/LocalConfig.elm
# Change module header from "LocalConfig.Example" to "LocalConfig"
ddev gulp
```

Default credentials (created by migration): pairing code `12345678`, nurse PIN `1234`, Drupal admin `admin`/`admin`.

## Code Conventions

### Alphabetical Ordering in Elm

Union type variants, case branches, and pattern matches must be in **alphabetical order**. This applies throughout the codebase, including `Backend/Model.elm` (Revision type), `SyncManager/Model.elm` (BackendAuthorityEntity type), and all corresponding decoders, encoders, update handlers, and view functions.

### Files to Never Commit

Do not commit `.ddev/.gitignore` or other DDEV-generated config files. These are local environment files and should not be included in commits.

### Translations in Elm

Translations in `Translate.elm` use a record with `english`, `kinyarwanda`, `kirundi`, and `somali` fields. When a non-English field is `Nothing`, it falls back to the English value at runtime. Therefore, if a translation would be identical to English (e.g., unit abbreviations like "kg", "cm", "MUAC"), leave it as `Nothing` — do not set it to `Just "kg"` etc.

### Debug Code in Elm

When committing Elm files, do not include `Debug.log` calls. Remove any `Debug.log` lines before staging.
