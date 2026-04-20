# Translate.elm — follow-up items

Items surfaced during the `refactor/translate-elm-dedup` work that need
review beyond the scope of that PR. None block normal use of E-Heza;
all are quality / cleanliness opportunities.

For context: the dedup refactor PR collapsed 252 duplicate-translation
sites across 126 safe groups into `translationSet <Anchor>` dispatches.
This doc lists the items it could NOT touch safely.

## Resolved follow-ups

Items that have since been fixed in subsequent commits on the same
branch (`refactor/translate-elm-dedup`):

| english | resolution | commit |
|---|---|---|
| Other | Added top-level `OtherLabel` anchor with canonical translations (en="Other", rw="Ibindi", rn="Ibindi", so="Kale") and refactored all 23 `*Other` case branches to dispatch via it. | `b41595799` |

## 1. Diverging duplicate-english groups (need native-speaker review)

These **125 remaining groups** (126 originally; 1 resolved — see
*Resolved follow-ups* above) have the same English label but different
translations in at least one of rw/rn/so across their duplicate
constructors. They were NOT refactored to `translationSet` because
doing so would change the UI translation for some users. A
native-speaker reviewer should pick the canonical per-locale
translation for each group, then a follow-up commit can dedup them
the same way as `Other` was resolved.

| english | constructors | divergence |
|---|---|---|
| Abdomen | `Abdomen`, `PainAbdomen` | rw differs (2 variants), so differs (2 variants) |
| Poor Suck | `AcuteFindingsPoorSuck`, `PoorSuck` | rn differs (2 variants) |
| Respiratory Distress | `DangerSignRespiratoryDistress`, `RespiratoryDistress` | rn differs (2 variants) |
| Bloody Diarrhea | `DangerSignBloodyDiarrhea`, `BloodyDiarrhea` | rn differs (2 variants) |
| Simple Cold and Cough | `DiagnosisSimpleColdAndCough` (×2 — different outer dispatches) | rw differs (2 variants) |
| Suspected COVID-19 case | `DiagnosisCovid19Suspect`, `SuspectedCovid19CaseAlert` | rw differs (2 variants) |
| Acute Illness History | `AcuteIllnessHistory`, `ComponentWellChildActiveDiagnoses` | rw differs (2 variants), so differs (2 variants) |
| Active Diagnosis | `ActiveDiagnosis`, `ComponentNCDActiveDiagnosis` | rw differs (2 variants), rn differs (2 variants) |
| Referred to Health Center | `OutcomePatientDied`, `ActionReferredToHealthCenter` | rw differs (2 variants) |
| Family Planning | `ActivitiesTitle`, `NCDActivityTitle`, `FilterFamilyPlanning` + 1 more | rw differs (2 variants) |
| Physical Exam | `AcuteIllnessPhysicalExam`, `PhysicalExam` | rw differs (2 variants) |
| Laboratory | `AcuteIllnessLaboratory`, `NCDActivityTitle`, `PrenatalPhoto` | rn differs (2 variants) |
| ... (113 more rows; see `/tmp/translate-dedup-groups.json`'s `diverging` array) | | |

Notable rows worth pulling out:

- **"Simple Cold and Cough"** has the same constructor name `DiagnosisSimpleColdAndCough` appearing twice — once in `AcuteIllnessDiagnosis option ->` and once in `AcuteIllnessDiagnosisWarning option ->`. These are not unreachable code (different parent dispatches), but having different `rw` translations across two parallel contexts is suspicious — likely one of the two is the "newer/canonical" version and they should agree.
- **Vaccine names** (`BCG`, `HPV`, `IPV`, `Measles-Rubella`, `OPV`, `PCV 13`, `Rotarix`, `Pentavalent`) all diverge between `SiteBurundi` and `Vaccine*` constructors on `rn` (Kirundi). Could be intentional (Burundi-specific Kirundi spellings vs general Kinyarwanda-leaning forms) but worth reviewer confirmation.

The full list lives in the analysis output JSON; the dedup PR's commit message points at this doc.

## 2. Skipped during dedup refactor

### 2a. Anchor-naming infeasible (20 cases)

Cases where the proposed PascalCase anchor name would be:
- Empty (english is `""` or single non-alphanumeric char)
- Digit-leading (would be invalid Elm constructor name)
- > 50 characters (e.g., long "1 tablet by mouth twice a day" style strings)
- All-punctuation (`+`, `++`, `0`, `1`, `2`, `4`)

| english | constructors |
|---|---|
| `Explain to the mother how to check the malnutrition signs fo` (long) | `ChildActivity`, `NutritionActivityHelper`, `NutritionHelper` |
| `Calibrate the scale before taking the first baby's weight. P` (long) | `ChildActivity`, `NutritionActivityHelper` |
| `+` | `BloodSmearPlus`, `NitritePlus` |
| `++` | `BloodSmearPlusPlus`, `NitritePlusPlus` |
| `` (empty) | `DeviceNotAuthorized`, `EmptyString`, `PageMain` + 1 more |
| `<20 copies` | `ResultSuppressedViralLoad`, `LabResultsHistoryHIVPCR` |
| `0` | `Protein0`, `Glucose0`, `LabResultsHistoryProtein` + 1 more |
| `+1` | `ProteinPlus1`, `GlucosePlus1` |
| `+2` | `ProteinPlus2`, `GlucosePlus2` |
| `+3` | `ProteinPlus3`, `GlucosePlus3` |
| `+4` | `ProteinPlus4`, `GlucosePlus4` |
| `1` | `Urobilinogen10`, `Ubudehe1` |
| `2` | `Urobilinogen20`, `Ubudehe2` |
| `4` | `Urobilinogen40`, `Ubudehe4` |
| `1 tablet by mouth twice a day` | `OutsideCareMedicationMethyldopa2`, `TreatmentMethyldopa2`, `TreatmentNifedipine` + 2 more |
| `2 capsules by mouth 3 times a day for 7 days` | `TreatmentCloxacillin`, `TreatmentMastitisAmoxicillin` |
| `1 tablet by mouth 3 times a day for 5 days` | `TreatmentParacetamol`, `TreatmentIbuprofen` |
| `2 tablets by mouth in the morning and 1 tablet by mouth in t` (long) | `TreatmentMetformin2m1e`, `TreatmentGlipenclamide2m1e` |
| `2 tablets by mouth twice a day` | `TreatmentMetformin2m2e`, `TreatmentGlipenclamide2m2e` |
| `If distributed amount is not as per guidelines, select the r` (long) | `ChildActivity`, `MotherActivity` |

These could be refactored if the team is willing to mint anchors with explicit human-chosen names (e.g., `ProteinPlus1` could anchor to a new `LabPlusOne` TranslationId; `1 tablet by mouth twice a day` could anchor to `DoseOneTabletTwiceDaily`). Out of scope for the mechanical refactor.

### 2b. Anchor name clashes with existing top-level constructor (2 cases)

Cases where the proposed PascalCase anchor name collides with an
already-existing top-level `TranslationId` whose translations DIFFER
from the duplicate group's. Refactoring would require either renaming
the existing constructor (changes call sites) OR using a `Label` suffix
on the new anchor — but unlike the 17 import-clash cases that DID get
`Label`-suffixed in the dedup PR, these would also need linguistic
review since the existing anchor's translations are themselves
candidates for canonicalisation.

| english | clash | duplicate constructors |
|---|---|---|
| None of the Above | clash with existing top-level `NoneOfTheAbove` | `NoMedicationCausingHypertension`, `NoMedicalConditions`, `NoMedicationTreatingDiabetes`, `NoMedicationTreatingHypertension`, `NoNCDDangerSigns`, `NoNCDGroup1Symptoms`, `NoNCDGroup2Symptoms`, `NoNCDPainSymptoms` |
| Pregnancy Outcome | clash with existing top-level `PregnancyOutcome` | `BirthPlan`, `PregnancyOutcomeLabel` |

For "None of the Above": the existing `NoneOfTheAbove` has slightly different translations than the 8 `No*` constructors above. A reviewer should decide which translations are canonical, then dedup all 9 entries.

### 2c. Refactor sites with translation-mismatch within the safe group (handled in PR)

The dedup PR also skipped 35 individual sites at refactor time where
the literal's translations didn't byte-match the anchor's expected
translations (data inconsistencies that snuck through the
locale-equivalence check at group level — usually one record had
slightly different whitespace, escape chars, or an empty Just where
others had Nothing). These were left as literals to avoid silent
translation drift. They're listed in the dedup PR's Commit 5 message
body and don't need separate documentation here.

## 3. Constructor-rename candidates (typos)

Bugs revealed during exploration that involve renaming Elm
constructors. Out of scope for the dedup PR because renames change call
sites across the codebase; would need a focused PR each.

| typo'd constructor | suggested rename | english | reasoning |
|---|---|---|---|
| `NitritionSigns` | `NutritionSigns` | "Nutrition Signs" | clear typo (Nitrition → Nutrition) |
| `ResilienceSurveyTotalNumber0to1` etc. | various | n/a | check for similar typos in the resilience module |

The `NitritionSigns` typo is the only confirmed one from the dedup
exploration — others may surface during a careful read of `Translate.elm`.

## 4. Empty-string english groups

Groups where english is `""`. Likely intentional in some contexts
(e.g., `EmptyString` is a sentinel; `DeviceNotAuthorized` and
`PageMain` may use empty strings as fallback placeholders that get
filled by the rendering layer). Listed in §2a above and safest to
leave alone.

## 5. Cross-check tool limitation (informational)

The dedup refactor's verification tool (`/tmp/translate-cross-check.py`,
not committed) had a known limitation: when an arg-taking outer
constructor (e.g., `PrenatalDiagnosis diagnosis ->`) gets its FIRST
inner case branch refactored from a literal to a `translationSet`
dispatch, the tool's "find first literal in body" heuristic now picks
the SECOND inner case branch as "first" and reports an apparent diff.

This produced 20 false-positive "diffs" during the refactor, all
manually verified to be benign (the affected dispatches resolve to
anchors with byte-identical translations). The Elm compile + manual
verification confirmed zero real translation drift.

A future cross-check tool that resolves nested `translationSet`
dispatches transitively (rather than relying on first-literal
heuristic) would eliminate the false positives. Not blocking; just
something to note if the verification tooling gets reused.

## Source data

- `/tmp/translate-dedup-groups.json` — canonical analysis output (not
  committed; regenerate with `/tmp/translate-dedup-analysis.py` if
  needed)
- Branch: `refactor/translate-elm-dedup`
- Commits: `0142c00f8` (spec/plan), `9e282eb21` (anchors), `30d1778a7`
  (refactor)
