# E2E Testing Knowledge Base — E-Heza

Detailed reference for writing Playwright E2E tests. See `SKILL.md` for the workflow.

---

## 1. Infrastructure

### Stack
- **Framework:** Playwright (TypeScript)
- **Config:** `client/playwright.config.ts`
- **Test dir:** `client/e2e/`
- **Helpers:** `client/e2e/helpers/`
- **Viewport:** iPad Mini (devices preset), `hasTouch: false`, `isMobile: false`
- **Workers:** 1 (serial execution — tests share a single device/pairing code)
- **Timeout:** 120s per test (global default; `test.setTimeout(600000)` for multi-encounter)
- **Retries:** 1
- **Recording:** `RECORD=1` env var enables headed mode, video, cursor injection, hover-before-click delays

### Device Lifecycle
- `global-setup.ts` creates a fresh "E2E Test Device" with pairing code `99999999` via drush
- `resetDevice()` (from `helpers/device.ts`) deletes and recreates the device — must be called in `beforeEach` since pairing codes are single-use
- Uses `hedley_super_user_mode` temporarily to bypass deletion restrictions
- Detects ddev vs host environment via `existsSync` check for settings.php path

---

## 2. Available Test Accounts (from migration CSV)

| Name | Role | PIN | Health Centers | Villages |
|------|------|-----|----------------|----------|
| Maya | Nurse | 1234 | Nyange, Muhondo | — |
| Jojo | CHW | 2345 | Nyange, Muhondo | Akanduga, Busake |
| PhePhe | Nurse | 4321 | Nyange, Muhondo | — |
| YumYum | CHW | 5432 | Nyange, Muhondo | Akanduga, Busake |
| LabTech | Lab Tech | 3333 | Nyange, Muhondo | — |

---

## 3. Selector Reference

### Page Selectors
| Page | Selector |
|------|----------|
| Clinical | `div.page-clinical` |
| Encounter Types | `div.page-encounter-types` |
| Participants | `div.page-participants` |
| Participant (individual) | `div.page-participant.individual.{type}` |
| Encounter | `div.page-encounter.{type}` |
| Activity | `div.page-activity.{type}` |

Replace `{type}` with: `nutrition`, `prenatal`, `well-child`, `ncd`, `hiv`, `tuberculosis`, `acute-illness`.

### Activity Card Icons
Format: `div.icon-task-{name}` — click to open activity.

**Nutrition:** `height`, `weight`, `muac`, `nutrition`, `photo`, `next-steps`
**Well Child:** `danger-signs`, `nutrition-assessment`, `immunisation`, `ecd`, `medication`, `home-visit`, `next-steps`, `history` (PregnancySummary), `photo`, `ncda`
**Prenatal:** `pregnancy-dating`, `history`, `examination`, `family-planning`, `danger-signs`, `symptoms`, `malaria-prevention`, `mental-health`, `immunisation`, `medication`, `laboratory`, `next-steps`, `birth-plan`, `breastfeeding`, `speciality-care`, `treatment-review`
**NCD:** `danger-signs`, `symptom-review`, `examination`, `family-planning`, `medical-history`, `laboratory`, `outside-care`, `next-steps`
**HIV:** `diagnostics`, `medication`, `symptoms`, `next-steps`

### Form Input Selectors
```typescript
// Text input by label (no name attribute)
page.locator('.ui.grid').filter({ hasText: 'Label:' }).locator('input').first()

// Yes/No radio (CSS-hidden, click label)
page.locator('.form-input.yes-no.{fieldClass} label', { hasText: 'Yes' })

// Checkbox
page.locator('.ui.checkbox label', { hasText: /^Option Text$/i })

// Checkbox in specific form
page.locator('{formSelector} .ui.checkbox', { hasText: /^Option$/i }).locator('label')

// Dropdown by label
page.locator('.ui.grid').filter({ hasText: 'Label:' }).locator('select').first()
```

### Button Selectors
```typescript
// Save (active/enabled)
page.locator('button.ui.fluid.primary.button.active')

// Save (disabled — do NOT click, no onClick handler)
page.locator('button.ui.fluid.primary.button.disabled')

// End Encounter
page.locator('div.actions button.ui.fluid.button', { hasText: 'End Encounter' })

// Confirmation dialog
page.locator('div.ui.tiny.active.modal button.ui.primary.fluid.button')
```

### Sub-Task Tab Icons
```typescript
// Click a sub-task tab
page.locator('.link-section:has(.icon-activity-task.icon-{name})')

// Iterate all visible tabs
const tabs = page.locator('#tasks-bar .icon-activity-task');
const count = await tabs.count();
```

**HIV sub-task icons:**
- Medication: `icon-medication` (PrescribedMedication), `icon-treatment-review` (TreatmentReview)
- NextSteps: `icon-next-steps-health-education`, `icon-next-steps-follow-up`, `icon-next-steps-send-to-hc` (Referral)

### Calendar Popup
```typescript
// Open
page.locator('.date-input')

// Popup container
page.locator('.ui.active.modal.calendar-popup')

// Year select
page.locator('div.calendar > div.year > select')

// Month select (1-indexed: "1" = January)
page.locator('div.calendar > div.month > select')

// Day cells (exclude dimmed)
page.locator('div.calendar table tbody td:not(.date-selector--dimmed)')

// Save (div, not button)
popup.locator('div.ui.button')
```

### Diagnosis Popup
```typescript
page.locator('div.ui.active.modal.diagnosis-popup')
// Continue button inside:
popup.locator('button.ui.primary.fluid.button')
// Use force: true and verify hidden after dismiss
```

---

## 4. Navigation Flows

### Dashboard -> Encounter
1. Dashboard -> click `.icon-task-clinical` -> `div.page-clinical`
2. Click `button.individual-assessment` -> `div.page-encounter-types`
3. Click encounter type button -> `div.page-participants`
4. "Register a new participant" button -> registration form
5. Fill form -> submit -> `div.page-participant.individual.{type}`
6. Click encounter button -> `div.page-encounter.{type}`

### End Encounter
1. `await page.waitForTimeout(2000)` (let Elm finish re-rendering)
2. Click `div.actions button.ui.fluid.button` with text "End Encounter"
3. Confirmation dialog: `div.ui.tiny.active.modal`
4. Click confirm: `div.ui.tiny.active.modal button.ui.primary.fluid.button`

### Sync
1. Click `span.sync-icon`
2. Navigate to device status via `.icon-task-device-status`
3. Find HC section: `.health-center` containing `h2` with HC name
4. Click "Start Syncing" button if visible
5. Wait for `.sync-status` with text "Status: Success" (up to 120s)
6. Go back to previous page

---

## 5. Shared Helper API (never duplicate these)

### auth.ts
- `click(locator, page)` — hover-then-click in RECORD mode, regular click otherwise
- `pairDevice(page, code?)` — enter pairing code, wait for PIN page
- `login(page, pin?, location?)` — pair + PIN + location selection
- `setupDevice(page, pin?, location?)` — login + navigate to device status + sync

### device.ts
- `resetDevice()` — delete and recreate E2E test device via drush (synchronous)
- `drushEnv` — environment config for drush calls (cwd, encoding, timeout)

### cursor.ts
- `installCursorScript()` — returns JS for visual cursor overlay in recordings

---

## 6. Per-Module Helper API

### Common Pattern (all modules)
Each module helper exports:
- Patient creation function (returns `{ firstName, secondName, fullName }`)
- One function per activity (opens activity, fills form, saves, returns to encounter page)
- End encounter function
- Sync function (`syncAndWait`)
- Backend query function (retries on missing expected types)

### Existing Helpers
| File | Module | Key exports |
|------|--------|-------------|
| `nutrition.ts` | Nutrition | `createChildAndStartEncounter`, `enterHeight/Weight/Muac`, `enterNutritionSigns`, `saveActivity`, `endEncounter`, `syncAndWait`, `completeSendToHC/HealthEducation/ContributingFactors/FollowUp`, `queryBackendNodes` |
| `home-visit.ts` | Home Visit | `startHomeVisit`, `completeFeeding/Caring/Hygiene/FoodSecurity`, `endHomeVisit`, `queryHomeVisitNodes` |
| `well-child.ts` | Well Child | `createChildAndStartWellChildEncounter`, `completeDangerSigns`, `completeNutritionAssessment`, `completeECD`, `completeMedication`, `completeImmunisation`, `completePregnancySummary`, `completeHomeVisit`, `completeNextSteps`, `endWellChildEncounter`, `queryWellChildNodes` |
| `prenatal.ts` | Prenatal | `createAdultFemaleAndStartEncounter`, `startPrenatalEncounter`, `navigateToParticipantPage`, `endPrenatalEncounter`, `backdatePrenatalEncounter`, 19+ activity helpers, `queryPrenatalNodes` |
| `acute-illness.ts` | Acute Illness | Similar pattern to well-child |
| `ncd.ts` | NCD | `createAdultAndStartNCDEncounter`, `completeDangerSigns/SymptomReview/Examination/FamilyPlanning/MedicalHistory/Laboratory/OutsideCare/NextSteps`, `endNCDEncounter`, `backdateNCDEncounter`, `navigateToParticipantPage/ToCaseManagement`, `queryNCDNodes` |
| `hiv.ts` | HIV | `createAdultAndStartHIVEncounter`, `completeDiagnostics/Medication/SymptomReview/NextSteps`, `endHIVEncounter`, `backdateHIVEncounter`, `navigateToParticipantPage`, `queryHIVNodes`, `syncAndWait` |

---

## 7. Backend Verification Patterns

### Key Rules
- **Base64-encode person names** — prevents shell injection with special characters
- **Person title format is reversed** — Drupal stores `"secondName firstName"` (e.g., `"Doe Jane"`)
- **Retry on missing types** — handles eventual consistency after sync (up to 10 attempts with 2s delay)
- **Assert every expected type** — if a type is expected but not asserted, missing writes won't fail the test
- **Assert negative cases** — verify conditional types that should NOT exist (e.g., `ncd_family_planning` for male patients)

### Query Pattern
See `client/e2e/helpers/ncd.ts` `queryNCDNodes()` for the canonical implementation:
1. Base64-encode person name
2. Use drush to run PHP that queries nodes by person title, then groups by content type
3. Retry loop: check if all expected types are present, sleep 2s and retry if not
4. Return record mapping content type -> boolean (exists or not)

---

## 8. Role Differences

### Registration
| Field | Nurse | CHW |
|-------|-------|-----|
| Address dropdowns | Required (Province->District->Sector->Cell->Village) | NOT rendered (auto-filled) |
| Health Center | Required dropdown | NOT rendered |
| Mode of delivery | Required | Required |
| Basic fields | All | All |

### Location Selection After Login
- **Nurse:** selects Health Center (button text = center name)
- **CHW:** selects Village (button text = village name)
- Both use `p.select-location` prompt

### Activity Differences
Activity availability differs by role. Always check `getAllActivities` in the encounter's `Utils.elm` for each role variant.

---

## 9. Elm SPA Quirks (Detailed)

### DOM Detachment from Re-renders
Elm's virtual DOM can detach elements between renders. Mitigate:
- `await page.waitForTimeout(2000)` before buttons after page transitions
- `element.click({ force: true })` for potentially-detaching elements

### Save Button Disabled State
Elm renders disabled buttons WITHOUT onClick handlers (not a no-op). Clicking does literally nothing.
- Active: `button.ui.fluid.primary.button.active`
- Disabled: `button.ui.fluid.primary.button` with class `disabled`

### Conditional Form Fields
Same activity can have different fields by encounter variant. Check `Activity/Utils.elm` for `ifNullableTrue`, `maybeToBoolTask`, and similar patterns.

### Sub-Task Tab Iteration
Activities with sub-tasks show different tabs by clinical state. Iterate visible tabs:
```typescript
const tabs = page.locator('#tasks-bar .icon-activity-task');
const count = await tabs.count();
for (let i = 0; i < count; i++) { /* click tab, fill, save */ }
```

### Progress Report Auto-Navigation
After NextSteps in Well Child, app auto-navigates to Progress Report. Loop `goBack()` to return:
```typescript
for (let i = 0; i < 5; i++) {
  if (await page.locator('div.page-encounter.{type}').isVisible()) break;
  await page.goBack();
  await page.waitForTimeout(1000);
}
```

---

## 10. CI Configuration

Two parallel Playwright CI jobs in `.circleci/config.yml`:
- **`e2e_playwright_1`**: runs `npx playwright test --grep-invert "..."` (excludes newer tests)
- **`e2e_playwright_2`**: runs `npx playwright test {specific-files}` (newer tests)

### Adding New Module Tests
1. Add spec file(s) to `e2e_playwright_2` command
2. Add grep-invert pattern to `e2e_playwright_1` to exclude the new tests
3. Both jobs share the same setup (ddev, install, migrations, etc.)

---

## 11. Lessons Learned

### Race Conditions After Page Transitions
After activity redirects, encounter page re-renders. Clicking too early lands on wrong activity. Fix: always wait for encounter page container + 500ms delay.

### Simple Locators Over Complex JS
When clicks "don't persist" (Save stays disabled), root cause is usually wrong page state or missing fields. Don't reach for `page.evaluate()`. Debug with screenshots.

### Multi-Encounter Test Pattern
For subsequent/postpartum encounters:
1. Complete prerequisite encounter -> end -> sync
2. Backdate encounter via drush
3. Sync again (backdate propagates)
4. Navigate back -> start new encounter -> complete -> end -> sync -> verify

### Build Incrementally
Start with simplest test (CHW, fewest activities). Get passing, then build up to complex variants.

### Medication Icon Gotchas
Vitamin A uses icon `treatment-review` (NOT `vitamin-a`). Always verify in `Activity/View.elm`.

### End-Encounter Dialog from Activity Save
HIV Diagnostics has a built-in end-encounter confirmation dialog. When no HIV diagnosis is established (patient refuses test or gets negative result), clicking Save triggers a `div.ui.tiny.active.modal` that closes the encounter and navigates away — bypassing the normal "End Encounter" button flow. Other encounter types may have similar activity-level dialogs.

### "None" Text Variants
- `"None of these"` (NoneOfThese translation) — symptom review
- `"None of These"` — nutrition signs
- Check exact text in `Translate.elm` when in doubt

---

## 12. Test File Structure

```
client/e2e/
├── global-setup.ts                        # Creates E2E device
├── global-teardown.ts                     # Converts webm->mp4 (RECORD)
├── nutrition-encounter.spec.ts            # Nurse nutrition
├── nutrition-encounter-chw.spec.ts        # CHW nutrition
├── home-visit.spec.ts                     # CHW home visit
├── prenatal-encounter-nurse.spec.ts       # Nurse prenatal (3 variants)
├── prenatal-encounter-chw.spec.ts         # CHW prenatal
├── acute-illness-encounter-nurse.spec.ts  # Nurse acute illness
├── acute-illness-encounter-chw.spec.ts    # CHW acute illness
├── well-child-encounter-nurse.spec.ts     # Nurse well child
├── well-child-encounter-chw.spec.ts       # CHW well child
├── ncd-encounter-nurse.spec.ts            # Nurse NCD (4 tests)
├── hiv-encounter-chw.spec.ts              # CHW HIV (3 tests)
├── lab-tech-encounter.spec.ts             # Lab tech
└── helpers/
    ├── auth.ts        # Login/setup + click()
    ├── cursor.ts      # Visual cursor for recordings
    ├── device.ts      # Device reset via drush
    ├── nutrition.ts   # Nutrition helpers + syncAndWait()
    ├── home-visit.ts  # Home visit helpers
    ├── prenatal.ts    # Prenatal helpers (19+ activities)
    ├── acute-illness.ts
    ├── well-child.ts
    ├── ncd.ts         # NCD helpers (nurse template)
    └── hiv.ts         # HIV helpers (CHW template)
```

---

## 13. Admin Reports Testing (Statistical Queries / Completion)

### Key Differences from Encounter Tests

Admin reports use a **separate Elm application** (`server/elm/`) embedded in Drupal admin pages. This is fundamentally different from the client PWA:

| Aspect | Encounter Tests | Admin Report Tests |
|--------|----------------|-------------------|
| App URL | `http://localhost:3000` | `https://{site}/admin/reports/statistical-queries/` |
| Auth | Device pairing + nurse PIN | Drupal admin login (`admin`/`admin`) |
| Elm source | `client/src/elm/` | `server/elm/src/` |
| Data | Created during test (encounters) | Pre-seeded via drush scripts |
| Offline/sync | Yes (PWA) | No (server-rendered) |
| Page selectors | `div.page-encounter.{type}` | `.page-content.reports-menu`, `.page-content.reports` |

### Data Seeding (required before tests)

Two drush scripts must run in order:
```bash
# Phase 1: Generate per-person report data
drush scr profiles/hedley/modules/custom/hedley_reports/scripts/generate-data-for-all.php

# Phase 2: Aggregate into report_data nodes for large scopes
drush scr profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-large-datasets.php
```

### Statistical Queries Flow

**Menu page** (`/admin/reports/statistical-queries`):
- Page class: `.page-content.reports-menu`
- Scope dropdown: Global, Demographics (geo drill-down), Health Center
- "Load Data" button navigates to results page (full page reload)

**Results page** (`/admin/reports/statistical-queries/all|demographics/...|health-center/...`):
- Page class: `.page-content.reports`
- Top bar: "New Scope" button + scope label
- Report type dropdown: Acute Illness, Prenatal, Prenatal Diagnoses, Demographics, Nutrition
- Date range inputs (Start Date, Limit Date) — except Nutrition which uses all-time data
- Demographics report has "Download CSV" button

### Report Types

| Report | Content | Date Range | Special |
|--------|---------|-----------|---------|
| Demographics | Age/gender tables + encounter counts | Yes | CSV download |
| Acute Illness | Diagnosis × encounter type breakdown | Yes | 15 diagnosis types |
| Prenatal | Pregnancy outcomes, delivery location, ANC visits by trimester | Yes | — |
| Prenatal Diagnoses | 50+ diagnosis counts | Yes | — |
| Nutrition | Prevalence/incidence tables (stunting/wasting/underweight) | No (all-time) | Monthly/quarterly/yearly |

### Key Elm Source Files (Server — Statistical Queries)
- `server/elm/src/Pages/ReportsMenu/View.elm` — scope selection UI
- `server/elm/src/Pages/Reports/View.elm` — all 5 report type views
- `server/elm/src/Pages/Reports/Model.elm` — ReportType enum
- `server/elm/src/Backend/Reports/Model.elm` — PatientData types
- `server/elm/src/Backend/Reports/Decoder.elm` — JSON decoders

---

## 14. Completion Report Testing

The Completion report is the second admin report type (alongside Statistical Queries). It tracks **activity completion rates** — for each encounter, which activities were expected vs actually completed.

### Completion vs Statistical Queries

| Aspect | Completion | Statistical Queries |
|---|---|---|
| URL | `/admin/reports/completion/` | `/admin/reports/statistical-queries/` |
| Menu page class | `.page-content.completion-menu` | `.page-content.reports-menu` |
| Results page class | `.page-content.completion` | `.page-content.reports` |
| Data storage | Encounter node `field_reports_data` | Person node `field_reports_data` |
| Auto-triggered | **No** — manual scripts only | Yes — hooks + AQ |
| report_data variant | `'completion'` | `'statistical-query'` |
| Scopes | Global, Health Center | Global, HC, Province, District |

### 11 Report Types

| Report | String ID | TakenBy Filter | CSS Class |
|---|---|---|---|
| Acute Illness | `acute-illness` | Nurse/CHW | `div.report.acute-illness` |
| Prenatal | `prenatal` | Nurse/CHW | `div.report.prenatal` |
| Well Child (SPV) | `well-child` | Nurse/CHW (inferred) | `div.report.well-child` |
| Newborn Exam | `newborn-exam` | CHW only (hidden) | `div.report.well-child` |
| NCD | `ncd` | Nurse only (hidden) | `div.report.ncd` |
| HIV | `hiv` | CHW only (hidden) | `div.report.hiv` |
| Tuberculosis | `tuberculosis` | CHW only (hidden) | `div.report.tuberculosis` |
| Home Visit | `home-visit` | CHW only (hidden) | `div.report.home-visit` |
| Child Scoreboard | `child-scoreboard` | CHW only (hidden) | `div.report.child-scoreboard` |
| Nutrition Individual | `nutrition-individual` | Nurse/CHW | `div.report.nutrition-individual` |
| Nutrition Group | `nutrition-group` | Nurse/CHW | `div.report.nutrition-group` |

"Hidden" means TakenBy filter is not shown (exclusively one role).

### Table Structure

```html
<div class="section heading">Report Heading</div>
<div class="table wide">
  <div class="row"><!-- captions -->
    <div class="item row-label heading">Activity</div>
    <div class="item heading">Expected</div>
    <div class="item heading">Completed</div>
    <div class="item heading">%</div>
  </div>
  <div class="row"><!-- per activity -->
    <div class="item row-label value">Activity Name</div>
    <div class="item value">45</div>
    <div class="item value">40</div>
    <div class="item value">88.9%</div>
  </div>
</div>
```

### Data Pipeline for E2E Testing

**Critical:** Completion data is NOT auto-generated. After creating encounters through the PWA, you must run:

```bash
# Layer 1: Generate per-encounter completion data
ddev drush scr profiles/hedley/modules/custom/hedley_reports/scripts/completion-generate-{type}-data.php

# Layer 2: Aggregate into report_data nodes
ddev drush scr profiles/hedley/modules/custom/hedley_reports/scripts/completion-recalculate-large-datasets.php

# Clear cache
ddev drush cc all
```

### Key Elm Source Files (Server — Completion)
- `server/elm/src/Pages/CompletionMenu/View.elm` — scope selection (Global vs HC)
- `server/elm/src/Pages/Completion/View.elm` — all 11 report type views (751 lines)
- `server/elm/src/Pages/Completion/Model.elm` — ReportType enum, filter Model
- `server/elm/src/Pages/Completion/Utils.elm` — activity lists, string conversions
- `server/elm/src/Backend/Completion/Model.elm` — CompletionData, all activity enums
- `server/elm/src/Backend/Completion/Decoder.elm` — JSON decoder
- `server/elm/src/Backend/Completion/Utils.elm` — activity char↔enum mappings
- `server/elm/src/Pages/Components/View.elm` — `viewMetricsResultsTable` shared renderer
