---
name: e2e-acute-illness-lessons
description: 'Acute Illness E2E lessons: popups mid-activity, diagnosis-driven activity sets, adult/CHW tab differences, reload on "Not Found", icon classes'
metadata:
  node_type: memory
  type: project
---

# E2E Testing — Lessons Learned

## Popup & Overlay Handling

- **Two popup types exist**: `.ui.active.modal` (diagnosis assessment) and `.overlay` (alert, e.g., "Suspected Uncomplicated Pneumonia"). Both have a "Continue" button. The `dismissDiagnosisPopup` helper must check for both.
- **Popups can appear mid-activity**, not just between activities. For example, saving Vitals with elevated RR triggers a "Suspected Pneumonia" overlay *within* the Physical Exam activity, before the Acute Findings tab. Always dismiss popups after saving sub-task tabs, not just when opening activities.
- **Popups block clicks**. If a test hangs trying to click an icon or tab, the first thing to check is whether a popup is overlaying the page.

## Diagnosis-Driven Activity Availability

- **Available activities depend on the diagnosis**, not just the encounter type. Different symptom/vitals combinations produce different diagnoses, which in turn determine which activities (Laboratory, Next Steps, etc.) appear.
- **"Uncomplicated Pneumonia" (CHW)**: Only 3 activities — Symptoms, Physical Exam, Prior Treatment. No Laboratory or Next Steps.
- **Malaria path (fever + positive RDT)**: Full set — Symptoms, Physical Exam, Prior Treatment, Laboratory, Next Steps.
- **Key diagnosis thresholds for adults**:
  - Fever: body temp ≥ 37.5°C OR "Fever" symptom selected
  - Elevated RR: > 30 breaths/minute (adults)
  - Fever + negative RDT → "Fever of Unknown Origin"
  - No fever + respiratory symptoms + elevated RR → "Uncomplicated Pneumonia" / respiratory infection
  - No fever + respiratory symptoms + normal RR → "Simple Cold and Cough"

## Conditional UI Elements

- **Cough checkbox reveals a conditional radio** ("More than 2 weeks" / "2 weeks or less") that must be answered before the Save button enables.
- **Acute Findings tab only appears on initial encounters** (not subsequent). After saving the last visible tab, the form returns to the encounter page. Check `stillOnActivity` before trying to interact with it.
- **Core Exam tab is nurse-only** — CHW encounters skip it entirely.
- **MUAC and Nutrition tabs are children-only** — adult patients (≥18y) skip them.

## Subsequent Encounter Differences

- **Different activity set**: Danger Signs, Physical Exam, Ongoing Treatment (+ possibly Laboratory, Next Steps depending on diagnosis).
- **Ongoing Treatment icon class**: `ongoing-treatment` (not `treatment-review`). Always verify icon classes against Elm's `encodeActivityAsString` in `Backend/AcuteIllnessActivity/Utils.elm`.
- **Saving the last mandatory activity** does NOT return to the encounter page — it shows a diagnosis popup and auto-navigates to Next Steps. Use manual save + `dismissDiagnosisPopup` instead of `saveActivity` for the last mandatory activity.
- **Subsequent encounters end with Acute Illness Outcome page** (dropdown to select outcome like "Illness Resolved" or "Referred to Health Center"), not just an "End Encounter" button.

## Backend Verification

- **`queryAcuteIllnessNodes` returns `{}` if ANY expected type is missing** after retries. Only include content types you're certain the encounter will create. For example, don't include `malaria_testing` if there's no Laboratory activity.
- **Next Steps sub-tasks are diagnosis-dependent**. Don't assume `medication_distribution`, `acute_illness_follow_up`, or `health_education` nodes will always exist.

## "Not Found" / Data Not Loaded

- **First thing to try: page reload** (`page.reload()`). The Elm app fetches data lazily from IndexedDB via the service worker. If data hasn't been indexed yet after sync, the page shows "Not Found". A reload re-triggers the fetch and usually resolves it.
- **ClinicsPage "Not Found"**: Clinics data may not be in IndexedDB immediately after sync. Add a retry loop with `page.reload()` until the expected buttons appear.
- **GroupEncounterTypesPage "Child Nutrition" does nothing (NoOp)**: The `GroupEncounterTypes/Fetch.elm` returns an empty list — it doesn't trigger `FetchClinics`. The `getVillageClinicId` needs `db.clinics` loaded. For CHW flow, navigate back to Clinical page and reload to trigger data loading before retrying.
- **General pattern**: If clicking a button produces no navigation (NoOp), it likely means required data isn't loaded yet. Reload the page or navigate away and back to trigger the lazy fetch.

## Environment & Infrastructure

- **DDEV project must auto-resolve from `cwd`** — `drushEnv()` in `device.ts` resolves the project directory from `process.cwd()`. Never hardcode or manually set `E2E_DDEV_PROJECT`. If drush commands need to target a specific backend, fix the auto-resolution logic, don't work around it with env vars.
- **Pairing failures at test start** are usually environment issues (wrong backend URL, DDEV not running), not test code issues. Check that `Main.js` backendUrl matches the DDEV project where devices are created.
- **CHW login**: PIN `2345`, Village `Akanduga`. **Nurse login**: PIN `1234`, `Nyange Health Center`.

## General Test Writing Tips

- **Always run the test without recording first** — it's 3-4x faster and catches most issues. Use recording only for final verification.
- **Check screenshots on failure** — they reveal whether a popup is blocking, an activity is missing, or the page is in an unexpected state.
- **Activity availability changes with vitals/symptoms** — if you change test inputs (e.g., body temp, RR, symptom selection), the entire encounter flow may change. Always re-verify which activities appear after changing inputs.
- **CSS class for activity icons**: `.icon-task-{activityName}` where `activityName` comes from Elm's `encodeActivityAsString` (e.g., `AcuteIllnessOngoingTreatment` → `ongoing-treatment`, `AcuteIllnessPriorTreatment` → `prior-treatment`).

## Screen details

- **Adult patients** (≥18y): Physical Exam has only 3 tabs (Vitals, Core Exam, Acute Findings) — no MUAC or Nutrition tabs.
- **After all mandatory activities complete**: the app shows the diagnosis popup and navigates to Next Steps (it does NOT return to the encounter page).
- **After all Next Steps complete**: the app shows the Progress Report page with an "End Encounter" button.
- **Checkbox labels**: Acute Findings and Symptoms use "None of the above"; Nutrition uses "None of these".
- **CSS classes**: physical exam actions are `actions symptoms` (not `actions physical-exam`); prior treatment and laboratory actions are `actions malaria-testing`.
