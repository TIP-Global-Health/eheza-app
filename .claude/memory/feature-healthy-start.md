---
name: feature-healthy-start
description: What the Healthy Start feature flag changes in the Prenatal workflow (classification, GWG, ultrasound, progress report)
metadata:
  node_type: memory
  type: project
  originSessionId: cae69946-63fd-4a29-87e7-2fba182af972
---

"Healthy Start" is a feature flag (`FeatureHealthyStart` / `"healthy_start"`) toggled per-site that alters a handful of Prenatal-module behaviors. It is NOT a standalone program like HIV or NCD — it only tweaks Prenatal flows. The flag's comment in `SyncManager/Model.elm` is explicit: *"defines few slightly different behaviors at Prenatal flows."*

**Where it's used (key entry points):**
- `client/src/elm/Backend/Utils.elm` — `healthyStartEnabled` predicate
- `client/src/elm/Pages/Prenatal/Encounter/Utils.elm` — adds `Ultrasound` to expected nurse-encounter activities
- `client/src/elm/Pages/Prenatal/Activity/Utils.elm` — `resolvePrePregnancyClassification`, `resolveGWGClassificationForHealthyStart`, `weightGainStandardsByPrePregnancyClassificationHealthyStart`
- `client/src/elm/Pages/Prenatal/Activity/View.elm` — `viewNutritionAssessmentForm` has `isHealthyStart` branches
- `client/src/elm/Pages/Prenatal/ProgressReport/View.elm` + `ProgressReport/Svg.elm` — `viewWeightGainForEGAHealthyStart`

**What it changes functionally:**
1. **Ultrasound activity** — added to the nurse-encounter expected activities list.
2. **Pre-pregnancy nutritional classification** — uses booking MUAC + optional baseline BMI; flags `PrePregnancyUnderWeight` if `baselineBmi < 17.5` or MUAC is low. Standard (non-Healthy-Start) path instead uses baseline BMI + age-at-LMP (with special <19y handling).
3. **Baseline BMI source** — computed from booking weight (not pre-pregnancy weight). `viewBaselineBmi` is hidden in the nutrition assessment UI.
4. **Gestational Weight Gain (GWG)** — uses `resolveGWGClassificationForHealthyStart` with per-day gain standards that split across first-trimester vs other-trimesters (e.g., `(0.0235, 0.073)` for underweight). Base weight for GWG is the previous weight, not the pre-pregnancy weight.
5. **Progress Report chart** — renders `viewWeightGainForEGAHealthyStart` (different SVG curve) instead of the standard `viewWeightGainForEGA`.

**Why:** Healthy Start reflects a different clinical protocol variant adopted at some sites — classification uses MUAC (more practical in the field) and the GWG curves follow different per-trimester standards.

**How to apply:** When touching Prenatal nutrition-assessment, pre-pregnancy classification, GWG, or the prenatal progress-report weight chart, check both branches (`isHealthyStart` true/false). When adding new prenatal activities or classifications, consider whether Healthy Start needs a parallel variant.
