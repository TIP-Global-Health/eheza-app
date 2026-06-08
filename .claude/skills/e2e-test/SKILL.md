---
name: e2e-test
description: Create Playwright E2E tests for E-Heza encounter types and admin report pages. Trigger when user asks to write, create, or add e2e tests, playwright tests, or end-to-end tests for any encounter type (nutrition, prenatal, NCD, well-child, acute illness, HIV, tuberculosis, etc.), admin reports (statistical queries, completion), or other features. Also use when asked to add tests for a new health program module.
---

# E2E Test Skill for E-Heza

This skill guides you through creating correct Playwright E2E tests for the E-Heza health app. The app is an Elm SPA with many non-obvious quirks that cause test failures if not accounted for.

## Step 1: Mandatory Pre-Work — Read Elm Source

Before writing ANY test code, read these 5 Elm files for the target encounter type (replace `{Type}` with the module name, e.g., `NCD`, `Prenatal`, `WellChild`, `HIV`, `Tuberculosis`):

1. **`client/src/elm/Pages/{Type}/Encounter/Utils.elm`** — find `getAllActivities` to know which activities exist per encounter variant
2. **`client/src/elm/Pages/{Type}/Activity/View.elm`** — form structures, CSS classes, sub-task rendering
3. **`client/src/elm/Pages/{Type}/Activity/Utils.elm`** — sub-task logic, conditional fields, form validation
4. **`client/src/elm/Backend/{Type}Activity/Utils.elm`** — activity icon class mappings (`icon-task-{name}`)
5. **`client/src/elm/Backend/{Type}Activity/Model.elm`** — Drupal content type names for backend verification

Also check for role differences: search for `isChw` in the View/Utils files to understand nurse vs CHW behavior.

6. **`client/src/elm/Pages/IndividualEncounterTypes/View.elm`** — check the `isChw` branch to determine which roles can access this encounter type (e.g., HIV is CHW-only, NCD is nurse-only). This determines login credentials, registration flow, and encounter type button text.

## Step 2: Analyze the Encounter Type

Before writing code, determine:
- **Which roles** can access this encounter type? (from `IndividualEncounterTypes/View.elm`)
- What activities exist? (from `getAllActivities`)
- Are there encounter variants? (initial/subsequent/postpartum, role-based)
- Which activities are conditional? (gender, age, diagnosis, prior encounter)
- What triggers NextSteps? (abnormal values, diagnoses)
- What backend node types are created per activity?

### Test Design Principle: Cover All Content Types

Tests must follow paths that create **all** backend content types the module defines. List every node type from the Measurements model, then design test inputs that trigger creation of each one. If an activity is conditional (e.g., NextSteps triggered by abnormal values, or VaccinationHistory triggered by a specific NCDA answer), choose form values that activate it. A test that only verifies one node type out of many does not catch missing/broken sync for the others. The goal is one test that exercises all activities and verifies all expected node types exist in the backend after sync.

**Important:** Before assuming a content type is a test gap, check its `expectActivity`/`expectTask` condition in the Elm Utils. Some content types are **permanently disabled** (return `False` unconditionally) due to deprecated features. These cannot be tested and should be excluded from coverage analysis. Example: Prenatal `Social` history task was disabled per issue #1323.

## Step 2.5: Create Feature Branch

Before writing any test code, ask the user which branch to base on and create the feature branch:
```bash
git checkout -b e2e-tests-{type} {base-branch}
```

## Step 3: Create Helper File

Create `client/e2e/helpers/{type}.ts` following the structure of `client/e2e/helpers/ncd.ts` (canonical template).

Section order:
1. Imports (`Page` from Playwright, `click` from `./auth`, `drushEnv` from `./device`)
2. Private form helpers (copy-paste from ncd.ts, do NOT import from other modules)
3. Patient creation function
4. Activity helper functions (one exported function per activity)
5. Navigation helpers
6. Sync helper
7. Backend verification query

### Key Rules for Helper Files

1. **Private helpers are copy-pasted, not shared** — each module helper file is self-contained
2. **`selectByLabel` uses 1-based indexing** — index 1 selects the first real option (skips blank default)
3. **`formInput` locates by label in `.ui.grid`** — because Elm forms have no `name` attributes on inputs
4. **`answerYesNo` uses `.form-input.yes-no.{class} label`** — NOT `.bool-input` or `.form-input.bool-input`
5. **`openActivity` waits for encounter page + 500ms delay** — prevents race conditions from Elm re-renders
6. **Save button must have `.active` class** — disabled buttons have NO onClick handler in Elm
7. **Always use `click()` from auth.ts** — handles hover-before-click in RECORD mode
8. **Backend queries use base64-encoded names** — prevents shell injection with special characters
9. **Backend queries retry on missing types** — handles eventual consistency after sync

## Step 4: Create Spec File

Create `client/e2e/{type}-encounter-{role}.spec.ts` following the structure of `client/e2e/ncd-encounter-nurse.spec.ts` (canonical template).

Structure:
- RECORD cursor injection in separate `beforeEach` (guarded by `if (process.env.RECORD)`)
- `resetDevice()` + `setupDevice()` in main `beforeEach`
- Test accounts: Nurse PIN `1234`/`Nyange Health Center`, CHW PIN `2345`/`Akanduga`, LabTech PIN `3333`
- Multi-encounter tests: `test.describe.configure({ timeout: 600000 })`
- Backend verification: list expectedTypes, assert each, also assert negative cases
- Each test must have a comment block explaining context and purpose

### Test Comment Block Template

Every `test(...)` block should have a comment explaining context:

```typescript
// Scenario: Normal encounter with healthy values for male adult patient.
// Activities: DangerSigns, SymptomReview, Examination, MedicalHistory, Laboratory.
// Conditions: Male patient -> FamilyPlanning NOT shown. Normal values -> NextSteps NOT triggered.
// Backend: Verifies 9 node types created, confirms ncd_family_planning absent.
```

## Step 5: Commit, Open PR, Add Comments

1. Commit with descriptive message explaining what tests are added
2. Push and open PR with summary of scenarios covered and what's verified
3. **Post one PR comment per test** explaining:
   - Scenario description
   - Activities completed (with form values)
   - What's verified (use ✅ checkmarks for each node type asserted, both present and absent)

## Step 6: Run Tests

Run all new tests headless to verify they pass:
```bash
cd client && ./node_modules/.bin/playwright test {spec-file}
```

**Do NOT run with `RECORD=1` unless the user explicitly asks for it.**

**Always run in background** — E2E tests take minutes; never run them in the foreground. Use `run_in_background: true` and immediately show the trace path:
```
Trace: tail -f <output-file-path>
```

**Do NOT redirect output yourself with `>` or `2>&1`.** The Bash tool's `run_in_background: true` already captures stdout+stderr to a file and returns its path in the tool result (e.g. `/tmp/claude-.../tasks/<id>.output`). Use that path verbatim as the trace target. Adding your own `> /tmp/whatever.log 2>&1` is redundant and hides output from the harness's notification.

Concretely:
- ✅ `command: ./node_modules/.bin/playwright test reporting.spec.ts`, `run_in_background: true` — the tool result tells you where to tail.
- ❌ `command: ./node_modules/.bin/playwright test reporting.spec.ts > /tmp/e2e-runs/foo.log 2>&1`, `run_in_background: true` — redundant redirect.

**Avoid re-running long tests for debugging** — For tests > 2 minutes, never re-run the full test just to debug a selector, label, or assertion issue. Instead:
1. Check error context files (screenshots, page snapshots) from the failed run
2. Read the Elm/HTML source to verify selectors, class names, labels
3. Write tiny standalone scripts (Playwright one-liner, drush query) to test assumptions
4. Only re-run the full test when confident the fix is correct

## Step 7: Add Tests to CI

Update `.circleci/config.yml`:
- Add new test files to `e2e_playwright_2` job
- Update `e2e_playwright_1` `--grep-invert` to exclude the new tests

## Step 8: Push and Verify CI

Push changes and verify all CI jobs pass.

---

## 14 Critical Pitfalls

These are the most common mistakes when writing E-Heza E2E tests. Violating any of them WILL cause test failures.

### 0. Add CSS Identifiers in Elm Rather Than Loosening Tests
When you can't reliably target an element (duplicate labels, ambiguous structure, no distinguishing class), **add a CSS class in the Elm source** to serve as a test identifier. Never weaken assertions or use fragile index-based selectors as a workaround. The Elm codebase is under our control — adding a class like `class "encounter-row-ai-hc"` is cheap and makes tests robust.

### 1. No `name` Attributes on Form Inputs
Elm's `etaque/elm-form` does NOT set HTML `name` attributes. Locate inputs by label text:
```typescript
page.locator('.ui.grid').filter({ hasText: 'Label Text:' }).locator('input').first()
```
Only exceptions: `input[name="pincode"]` and `input[name="pairing-code"]`.

### 2. CSS-Hidden Radio/Checkbox Inputs
Actual `<input type="radio/checkbox">` elements are CSS-hidden by Semantic UI. Click the `<label>` instead:
```typescript
page.locator('.form-input.yes-no.field-class label', { hasText: 'Yes' })
```

### 3. Bool Input Class is `yes-no`, NOT `bool-input`
The Elm function `viewBoolInput` renders `div.form-input.yes-no.{fieldClass}`, NOT `form-input.bool-input`.

### 4. Reversed Person Title in Drupal
Drupal stores person names as `"secondName firstName"` (reversed from display). Backend queries must use this format.

### 5. Cascading Dropdown Waits
Province -> District -> Sector -> Cell -> Village dropdowns need `await page.waitForTimeout(500)` between each selection.

### 6. Diagnosis Popup Changes Navigation Flow
After abnormal assessment triggers a diagnosis popup, the app may auto-navigate to NextSteps (NOT back to the encounter page). Handle with:
```typescript
const popup = page.locator('div.ui.active.modal.diagnosis-popup');
if (await popup.isVisible({ timeout: 3000 }).catch(() => false)) {
  const btn = popup.locator('button.ui.primary.fluid.button');
  await btn.click({ force: true });
  await popup.waitFor({ state: 'hidden' });
}
```

### 7. Individual NextSteps Saves
Each NextSteps sub-task (SendToHC, HealthEducation, ContributingFactors, FollowUp) has its own tab, form, and individual save button. Don't try to save all at once.

### 8. Calendar Month Select is 1-Indexed
`div.calendar > div.month > select` uses string values `"1"` through `"12"`, NOT `"0"` through `"11"`.

### 9. Save Button Must Have `.active` Class
Elm renders disabled buttons WITHOUT an onClick handler. Clicking a disabled Save button does literally nothing. Always verify `.active` class before clicking.

### 10. Wait Before End Encounter
Always `await page.waitForTimeout(2000)` before clicking "End Encounter" to let Elm finish re-rendering after the last activity save.

### 11. "None" Text Variants
- Symptom review: `"None of these"` (from `NoneOfThese` translation)
- NOT `"None of the above"` or just `"None"`
- Nutrition signs: `"None of These"`
- Check exact text in `Translate.elm` when in doubt

### 12. Nurse vs CHW Registration Differences
- **Nurse:** Must fill cascading address dropdowns (Province -> District -> Sector -> Cell -> Village) + Health Center
- **CHW:** Address section NOT rendered (auto-filled). Only basic fields needed.

### 13. Single-Use Pairing Codes
Pairing codes are consumed on use. Always call `resetDevice()` in `beforeEach` to create a fresh device with code `99999999`.

### 14. Backend Query Retries for Eventual Consistency
After sync, backend nodes may not be immediately available. Retry queries with delay when expected types are missing.

### 15. Verify Checkbox/Label Text in Translate.elm
Always check the exact English text of checkbox labels, adverse events, symptoms, and medication names in `Translate.elm` before using them in selectors. Common trap: assuming punctuation (e.g., "Rash / Itching" vs actual "Rash or Itching"). A wrong label causes the locator to hang until timeout.

### 16. Encounter Type Availability Varies by Role
Not all encounter types are available to all roles. Check `Pages/IndividualEncounterTypes/View.elm` for the `isChw` branch:
- **Nurse-only:** NCD
- **CHW-only:** HIV, Tuberculosis (feature-flagged)
- **Both roles:** Acute Illness, Prenatal, Nutrition, Well Child

This determines login credentials, registration flow (address fields), and encounter type button text.

### 17. "Not Found" Page — Always Try `page.reload()` First
The Elm app fetches data lazily from IndexedDB via the service worker. After sync completes, some data may not be indexed yet, causing pages to show "The server indicated the following error: Not Found". The **first fix to try is always `page.reload()`** — this re-triggers the lazy fetch and usually resolves the issue. Wrap navigation in a retry loop:
```typescript
for (let attempt = 0; attempt < 5; attempt++) {
  const visible = await targetLocator.waitFor({ timeout: 10000 }).then(() => true).catch(() => false);
  if (visible) break;
  await page.waitForTimeout(2000);
  await page.reload();
}
```
This is especially common on the ClinicsPage (nurse group flow) and when `getVillageClinicId` needs `db.clinics` (CHW group flow). If a button click produces no navigation (NoOp), it likely means required data isn't loaded yet.

### 18. Read Elm Code Before Writing Selectors — Never Trial-and-Error
When a form or activity has complex behavior (conditional fields, mutually exclusive checkboxes, multi-section forms, popups), **always read the Elm View/Utils source first** to understand the full structure before writing Playwright selectors. Do NOT write selectors by guessing and iterating through test runs — each run takes 6-15 minutes. Specifically:
- Read the View function to know what CSS classes, input types, and element structure are rendered
- Read the Utils function to know what conditions show/hide fields, what makes Save active
- Check if answering one question reveals new questions (conditional rendering)
- Check if checkboxes are mutually exclusive within groups (treatment selections)
- Check what page the app navigates to after completing all activities (encounter page vs progress report)

### 19. Use Promise.race Timeout for Complex Inline Blocks
When writing multi-step form completion logic inline (not in a tested helper), wrap it in a `Promise.race` with a short timeout (e.g., 60s) during development. This fails fast instead of hanging for the full 15-minute test timeout when a selector doesn't match or a form gets stuck:
```typescript
await Promise.race([
  (async () => {
    // complex form completion logic here
  })(),
  new Promise((_, reject) =>
    setTimeout(() => reject(new Error('Block X timed out after 60s')), 60000),
  ),
]);
```
Remove the wrapper once the block is stable and passing.

---

## Reference

For detailed selector tables, helper signatures, account info, and CI configuration, see:
`~/.claude/skills/e2e-test/references/e2e-knowledge-base.md`

For canonical examples:
- **Helper template (nurse):** `client/e2e/helpers/ncd.ts` (complete, 4 activities + sub-tasks)
- **Helper template (CHW):** `client/e2e/helpers/hiv.ts` (CHW flow, no address fields, end-encounter dialog)
- **Spec template (nurse):** `client/e2e/ncd-encounter-nurse.spec.ts` (4 test patterns)
- **Spec template (CHW):** `client/e2e/hiv-encounter-chw.spec.ts` (3 tests: initial, subsequent, no-diagnosis)
- **Shared infra (never duplicate):** `client/e2e/helpers/auth.ts`, `device.ts`, `cursor.ts`
