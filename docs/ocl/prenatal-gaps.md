# Prenatal concepts without a PIH match

These concepts are captured by E-Heza during a Prenatal encounter but did not
resolve to a suitable concept in the Partners In Health (`PIH/PIH`) OCL
dictionary during the matching pass. They are **not** included in the pilot
`prenatal-concepts.csv` / `prenatal-mappings.csv` (per the "PIH-match-only"
rule for this phase).

For each gap, a suggested next action is proposed.

## Obstetrical exam

| Concept | E-Heza field | Suggested action |
|---|---|---|
| Fundus palpable (Boolean) | `ObstetricalExamValue.fundalPalpable` | Consider creating as a TIP-local Question mapped SAME-AS to a CIEL Boolean equivalent if found, or to the PIH fundal-height concept with a NARROWER-THAN map |

## Obstetric history

| Concept | E-Heza field | Suggested action |
|---|---|---|
| Number of preterm deliveries (distinct from stillbirths) | `ObstetricHistoryValue.preTermPregnancy` | No clean PIH Question concept. Search CIEL directly; if found, create TIP-local concept mapped to CIEL. Alternatively, request PIH add a counterpart to `PIH:1053` (Parity) |

## Breast exam

| Concept | E-Heza field | Suggested action |
|---|---|---|
| Breast discharge | `BreastExamSign.Discharge` | PIH search returned no "breast discharge" / "nipple discharge" concept. Try CIEL (common code ~164). Worth requesting from PIH |
| Breast warmth | `BreastExamSign.Warmth` | Generic "warmth" is a finding modifier, not a concept in PIH. Consider mapping to `PIH:13330` (Mastitis-Postpartum) with a NARROWER-THAN map if clinically appropriate, or create TIP-local |

## Physical exam

| Concept | E-Heza field | Suggested action |
|---|---|---|
| Hernia (generic) | `AbdomenCPESign.Hernia` | PIH has specific hernia types (inguinal, hiatal, etc.) but no generic "Hernia" finding. Either map to a CIEL generic-hernia concept or create TIP-local |

## Labs

| Concept | E-Heza field | Suggested action |
|---|---|---|
| Urine ketones (dipstick) | `UrineDipstickTestValue.ketone` | Not found in PIH search. CIEL has urine-ketones test concepts; create TIP-local mapped to CIEL |

## Low-confidence matches — excluded from pilot upload

These were identified during matching but **dropped before the pilot upload**
to `TIP/EHEZA`. Confirm or refine before re-introducing them in a later pass:

| E-Heza concept | PIH match assigned | Why low |
|---|---|---|
| Weight (kg) | `PIH:14397` Weight on admission (kg) | PIH has no generic "Weight (kg)" — only admission-context. May want a different PIH concept or request generic |
| Headache with blurred vision (compound danger sign) | `PIH:620` Headache | PIH has no compound "headache + blurred vision" concept; this only captures half the intent |
| Normal labor / Labor | `PIH:9329` Normal labor | PIH's concept implies normal progression; E-Heza's danger-sign `Labor` is "patient in labor (unexpectedly)" — semantic shift |
| Sinus tachycardia | `PIH:3242` Tachycardia | PIH has generic tachycardia but not sinus-specific |
| Hematuria (urine dipstick blood) | `PIH:840` Hematuria | PIH concept is the symptom/finding, not specifically a dipstick result |

## Deprecated / legacy concepts

These Prenatal concepts are flagged as deprecated in
`Backend/Measurement/Model.elm` (see `@todo` comments) and were intentionally
skipped for the pilot:

- `LastMenstrualPeriodValue.prePregnancyWeight` — deprecated
- `MedicationSign.DewormingPill` — deprecated (replaced by `Mebendazole`)
- `PreviousDeliverySign` — most constructors deprecated (replaced by `ObstetricHistoryStep2Sign`)
- `ObstetricHistorySign` — fully deprecated (replaced by `ObstetricHistoryStep2Sign`)

## Out of scope for this pilot

Captured during Prenatal but deferred to later iterations because they are
structurally complex (sub-records, multi-result sets, or encounter-level
diagnoses) rather than single-fact concepts:

- Full `PrenatalSymptomReview` subtree (~13 symptom constructors + follow-up questions)
- Full `PrenatalMentalHealthValue` PHQ-style screening questions (8 questions + options)
- `BreastfeedingValue` qualitative assessment fields
- Encounter-level `PrenatalDiagnosis` union (~60 constructors for diagnosis resolution)
- `OutsideCareValue` referral diagnoses/medications
- `MedicationDistributionValue` full catalog beyond aspirin/calcium/iron shortlist
- Birth plan signs, specialty care enrollment details, referral/follow-up workflow metadata
- Vaccination records (`PrenatalVaccineType`) — structurally similar to Well-Child immunisations; defer until the Well-Child pilot
