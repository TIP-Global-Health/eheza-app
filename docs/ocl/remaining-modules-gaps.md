# Gaps: Well-Child, Acute Illness, NCD, Tuberculosis, HIV, Family Nutrition

Concepts captured by E-Heza in these encounter types that did not land in the
per-module `*-concepts.csv` / `*-mappings.csv`, either because no suitable
PIH equivalent was found or because the concept is already represented by a
Prenatal / Nutrition concept already on `TIP/EHEZA`.

## Well-Child

**No PIH match**:

- `HPV vaccine` (`WellChildVaccineType.VaccineHPV`) — PIH returned no HPV-vaccine concept (code 1213 is HPV infection, not the vaccine).
- `Excessive sweating when feeding` (`WellChildSymptom.ExcessiveSweatingWhenFeeding`) — no diaphoresis / sweating concept.

**Dropped (low-confidence)**:

- `Severe edema` (`WellChildSymptom.SymptomSevereEdema`) — closest was `PIH:12636 Pitting edema`, but severity is not captured distinctly.
- `Baby tires quickly when feeding` (`WellChildSymptom.SymptomBabyTiresQuicklyWhenFeeding`) — `PIH:7930 Feeding problem` is too generic.
- `Coughing or tearing while feeding` (`WellChildSymptom.SymptomCoughingOrTearingWhileFeeding`) — also too generic in PIH.

**Already on OCL — reuse, not re-create**:

| E-Heza field | Reuses concept |
|---|---|
| `WellChildSymptom.SymptomBreathingProblems` | `EH-PRE-025 Difficulty breathing` |
| `WellChildSymptom.SymptomConvulsions` | `EH-PRE-023 Seizure` |
| `WellChildSymptom.SymptomDiarrhea` | `EH-NUT-008 Diarrhea` |
| `WellChildSymptom.SymptomVomiting` | `EH-AI-001 Vomiting` |
| `WellChildSymptom.SymptomHistoryOfFever` | `EH-PRE-026 Fever` |
| `WellChildSymptom.SymptomPalmoplantarPallor` | `EH-PRE-045 Pallor` (generic) |
| `WellChildVaccineType.VaccineTetanus` (prenatal tetanus via shared `PrenatalVaccineType`) | `EH-PRE-068 Td booster` |
| `WellChildHeight` / `WellChildMuac` / `WellChildWeight` / `WellChildNutrition` / `WellChildVitals` | existing Prenatal/Nutrition concepts |

**Out of scope (behavioural / program-specific)**:

- `ECDSign` — 40+ developmental-milestone constructors (FollowMothersEyes, Smile, SitWithoutSupport, KnowsColorsAndNumbers, etc.). These are program-specific ECD checklist items, not clinical observations standardised across OpenMRS/PIH.
- `VaccineDose` (1st…5th) — schedule ordinals, not concepts per se.
- `MeasurementNote` (NoteNotTaken) — structural flag.
- `AdministrationNote` (Mebendezole / VitaminA administration status) — dosing metadata.
- Well-Child health-education, contributing-factors, follow-up, send-to-HC — already handled via shared Nutrition concepts where applicable, otherwise encounter structure.

## Acute Illness

**No PIH match**:

- `DangerSignRespiratoryDistress` (composite) — represented via components (Dyspnea, Stridor, Nasal flaring).
- `DangerSignNewSkinRash` — `EH-HIV-020 Rash` covers the clinical concept.
- `AcuteFindingsRespiratorySign.NasalFlaring` — no concept found.
- `AcuteFindingsPoorSuck` — no dedicated concept, `EH-PRE-022` headache-alternate-form was dropped and poor-suck went to feeding problem (low).
- `AcuteFindingsRespiratorySign.SevereWheezing` — reuses `EH-PRE-041 Wheezes` (severity not distinctly coded on that concept).
- `SymptomsGISign.SymptomGIAbdominalPain` — reuses `EH-PRE-024 Abdominal pain`.
- `SymptomsGIDerivedSign.IntractableVomiting` — reuses `EH-AI-001 Vomiting` (severity not coded).
- `SymptomsGISign.NonBloodyDiarrhea` — reuses `EH-NUT-008 Diarrhea`.
- `RapidTestResult` / `MalariaTesting` — reuses `EH-PRE-057 Malaria RDT`.
- `TravelHistorySign.COVID19Country` — epidemiological flag, no clinical PIH concept.

**Dropped (low-confidence)**:

- `SymptomsGeneralSign.CokeColoredUrine` — `PIH:12289 Urine color` is a container test, not the specific dark-urine observation.
- `SymptomsGeneralSign.PoorSuck` / `DangerSignUnableDrinkSuck` — feeding-problem only at PIH.
- `SymptomsGeneralSign.DryMouth` — no xerostomia concept returned.

**Already on OCL — reuse**:

| E-Heza field | Reuses concept |
|---|---|
| `AcuteIllnessDangerSign.DangerSignConvulsions` | `EH-PRE-023 Seizure` |
| `AcuteIllnessDangerSign.DangerSignVomiting` | `EH-AI-001 Vomiting` |
| `AcuteIllnessDangerSign.DangerSignLethargyUnconsciousness` | `EH-WC-009 Lethargy` + `EH-PRE-031 Unconscious` |
| `AcuteIllnessDangerSign.DangerSignBloodyDiarrhea` | `EH-AI-018 Bloody diarrhea` |
| `AcuteIllnessDangerSign.DangerSignSpontaneousBleeding` | `EH-AI-010 Bleeding` |
| `AcuteFindingsGeneralSign.LethargicOrUnconscious` | `EH-WC-009 Lethargy` + `EH-PRE-031 Unconscious` |
| `AcuteIllnessNutrition` / `AcuteIllnessVitals` / `AcuteIllnessCoreExam` | shared Nutrition / Vital / CPE concepts |
| `SymptomsGeneralSign.SevereWeakness` | `EH-PRE-027 Weakness` |
| `SymptomsGeneralSign.SymptomsGeneralConvulsions` | `EH-PRE-023 Seizure` |
| `SymptomsGeneralSign.SymptomGeneralFever` / `HIVSymptomFever` | `EH-PRE-026 Fever` |

## NCD

**No PIH match**:

- `NCDDangerSign.FlankPain` / `NCDPainSymptom.PainFlank` — no concept.
- `NCDPainSymptom.PainFeet` — no foot-pain concept.
- `NCDGroup1Symptom.SwellingInLegs` — reuses `EH-PRE-047 Lower extremity edema`.
- `NCDGroup1Symptom.SwellingInFace` — no facial edema concept.
- `NCDGroup1Symptom.Palpitations` — no concept returned.
- `NCDGroup1Symptom.Tremor` — no standalone tremor concept.
- `NCDGroup1Symptom.MildHeadache` — reuses `EH-AI-005 Headache`.
- `MedicationCausingHypertension.MedicationOestrogens` / `MedicationSteroids` / `MedicationAmitriptyline` — not searched individually; many are not specific enough to map safely without per-drug review.
- `NCDSocialHistorySign.SignConsumeSalt` / `SignDifficult4TimesAYear` / `SignHelpWithTreatmentAtHome` — behavioural questions, not clinical facts.
- `Predecessor` (Father/Mother/GrandFather/GrandMother) — family-history relationship type, not a clinical concept.
- `NCDFamilyHistorySign.SignHypertensionHistory` / `SignHeartProblemHistory` / `SignDiabetesHistory` — family-history booleans; could be mapped to OpenMRS "History of … in family" questions in a later pass.
- `FoodGroup` (Vegetables/Carbohydrates/Protein) — dietary categories, social determinant.
- `NCDHealthEducationSign` — behavioural education flags.
- `NCDGuidanceSign.ReturnInOneMonth` — scheduling metadata.

**Dropped (low-confidence)**:

- `NCDGroup1Symptom.DizzinessWithChangingPosition` — `PIH:877 Dizziness and giddiness` is generic.
- `MedicationTreatingHypertension.MedicationCalciumChannelBlockers` — no class concept; `PIH:3187 Amlodipine` is a specific drug.
- `MedicationTreatingHypertension.MedicationBetaBlockers` — no class concept; `PIH:254 Propranolol` is a specific drug.

**Already on OCL — reuse**:

| E-Heza field | Reuses concept |
|---|---|
| `NCDDangerSign.Dyspnea` | `EH-AI-014 Dyspnea` |
| `NCDDangerSign.ChestPain` | `EH-AI-017 Chest pain` |
| `NCDDangerSign.Hematuria` | `EH-PRE-064` (was dropped) or add back — see NCD CSV which does not include a Hematuria row since it overlaps the prenatal dipstick dropped concept |
| `NCDGroup2Symptom.NCDIncreasedThirst` | `EH-AI-008 Excessive thirst` |
| `NCDCoreExam` | shared CPE concepts already on OCL |
| `NCDVitals` / `NCDUrineDipstickTest` / `NCDPregnancyTest` / `NCDHIVTest` / `NCDRandomBloodSugarTest` | Prenatal concepts |
| `NCDFamilyPlanning` | `EH-PRE-067` + `EH-NUT-009`…`EH-NUT-016` |
| `MedicalCondition.MedicalConditionPregnancy` | `EH-PRE-014 Currently pregnant` |

## Tuberculosis

**Already on OCL — reuse**:

| E-Heza field | Reuses concept |
|---|---|
| `TuberculosisSymptom.TuberculosisSymptomNightSweats` | `EH-AI-004 Night sweats` |
| `TuberculosisSymptom.TuberculosisSymptomBloodInSputum` | `EH-AI-011 Hemoptysis` |
| `TuberculosisSymptom.TuberculosisSymptomWeightLoss` | `EH-NCD-029 Weight loss` |

**Out of scope**:

- `TuberculosisDOTSign` (DOTPositive / DOTNegative* variants) — Directly Observed Therapy checkbox states; program-process data.
- `TuberculosisHealthEducationSign.EducationFollowUpTesting` — education flag.
- `MedicationOther` — placeholder.

## HIV

**No PIH match**:

- `HIVPrescribedMedication.HIVMedicationDarunavirCobicistat` — PIH has no DRV/c or Cobicistat concept.

**Dropped (low-confidence)**:

- `HIVSymptom.HIVSymptomHairLoss` — `PIH:10120 Alopecia areata` is a specific subtype, not generic alopecia.

**Already on OCL — reuse**:

| E-Heza field | Reuses concept |
|---|---|
| `HIVSymptom.HIVSymptomFever` | `EH-PRE-026 Fever` |
| `HIVSymptom.HIVSymptomFatigue` | `EH-TB-003 Fatigue` |
| `HIVSymptom.HIVSymptomSwollenLymphNodes` | `EH-PRE-040 Lymphadenopathy` |
| `HIVSymptom.HIVSymptomSoreThroat` | `EH-AI-015 Sore throat` |
| `HIVSymptom.HIVSymptomMuscleJointPain` | `EH-AI-002 Myalgia` |
| `HIVSymptom.HIVSymptomHeadache` | `EH-AI-005 Headache` |
| `HIVSymptom.HIVSymptomNightSweats` | `EH-AI-004 Night sweats` |
| `HIVSymptom.HIVSymptomDiarrhea` | `EH-NUT-008 Diarrhea` |
| `HIVSymptom.HIVSymptomWeightLoss` | `EH-NCD-029 Weight loss` |
| `HIVSymptom.HIVSymptomCoughingUpBlood` | `EH-AI-011 Hemoptysis` |
| `HIVSymptom.HIVSymptomDifficultyBreathing` | `EH-PRE-025 Difficulty breathing` |
| `HIVSymptom.HIVSymptomVomiting` | `EH-AI-001 Vomiting` |
| `HIVDiagnosticsValue.testResult` | `EH-PRE-054 HIV test result` |

**Out of scope**:

- `HIVDiagnosisSign.HIVResultPositiveReported` / `HIVResultPositiveKnown` / `HIVResultDateEstimated` / `HIVTestRun` — provenance flags.
- `HIVHealthEducationSign.*` — education flags.
- `HIVTreatmentSign.*` / `MedicationTreatmentSign.*` — adherence / side-effect flags.

## Family Nutrition

No new concepts. All Family Nutrition measurements are `MUAC` (mother / child) + `Photo`, already covered by `EH-PRE-008 MUAC` (for both) and the Photo concept (structural; not currently published).
