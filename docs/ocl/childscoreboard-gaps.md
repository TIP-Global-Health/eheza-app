# Child Scoreboard concepts — coverage map

The Child Scoreboard encounter (`Backend/ChildScoreboardActivity/Model.elm`) has
two activities — **NCDA** and **VaccinationHistory**. Almost every concept it
captures is already published from another encounter pass; this file documents
where each one lives so reviewers can see the full picture without grepping.

## New in this pass — `childscoreboard-concepts.csv`

| EH id | Concept | E-Heza field | PIH | Confidence |
|---|---|---|---|---|
| `EH-CS-001` | Birth weight (kg) | `NCDAValue.birthWeight` (grams) | `PIH:11067` | high |
| `EH-CS-002` | Number of antenatal visits for current pregnancy | `NCDAValue.ancVisitsDates` (set of dates) | `PIH:13321` | medium — E-Heza stores the dates, PIH concept is the derived count |

## Already represented in `TIP/EHEZA` — not re-created

### Vaccinations (8) — published in `wellchild-concepts.csv`

The `ChildScoreboardVaccinationHistory` activity captures the same eight
vaccines as the Well Child immunisation activity, just historically. Concepts
are reused 1:1 — no new rows.

| E-Heza measurement | Reuses concept | PIH |
|---|---|---|
| `ChildScoreboardBCGImmunisation` | `EH-WC-002` BCG vaccine | `PIH:886` |
| `ChildScoreboardOPVImmunisation` | `EH-WC-003` Oral polio vaccination | (see wellchild) |
| `ChildScoreboardDTPImmunisation` | `EH-WC-004` DTP vaccine | (see wellchild) |
| `ChildScoreboardDTPStandaloneImmunisation` | `EH-WC-004` DTP vaccine | (see wellchild) |
| `ChildScoreboardPCV13Immunisation` | `EH-WC-005` PCV13 | (see wellchild) |
| `ChildScoreboardRotarixImmunisation` | `EH-WC-006` Rotavirus vaccine | (see wellchild) |
| `ChildScoreboardIPVImmunisation` | `EH-WC-007` Inactivated polio vaccine | (see wellchild) |
| `ChildScoreboardMRImmunisation` | `EH-WC-008` Measles-Rubella vaccine | (see wellchild) |

### NCDA scalar fields — published elsewhere

| E-Heza field | Reuses concept |
|---|---|
| `NCDAValue.weight` | `EH-NUT-001` Weight (kg) |
| `NCDAValue.muac` | `EH-PRE-008` Mid-upper arm circumference (cm) |

### NCDA signs already mapped from the Nutrition pass

These `NCDASign` constructors are clinical signs already published in
`nutrition-concepts.csv`; the Child Scoreboard reuses them.

| `NCDASign` | Reuses concept |
|---|---|
| `BornWithBirthDefect` | `EH-NUT-019` Birth defects |
| `ChildGotDiarrhea` | `EH-NUT-008` Diarrhea |
| `ChildReceivesDewormer` | `EH-NUT-020` Deworming |
| `ChildWithDisability` | `EH-NUT-018` Disability |
| `ShowsEdemaSigns` | `EH-NUT-004` Edema |
| `ChildReceivesVitaminA` | `EH-PRE-051` Vitamin A (drug substance — same PIH concept) |

## Out of scope (programmatic / structural — same exclusions as Nutrition)

The full `NCDASign` programmatic / social-determinant set was already classified
out-of-scope in `nutrition-gaps.md` (see *Out of scope for this pilot*).
Items that reach Child Scoreboard via the shared `NCDAValue` and stay deferred:

- Supplements: `ChildReceivesFBF`, `ChildTakingFBF`, `OngeraMNP`, `TakingOngeraMNP`, `SupplementsDuringPregnancy`, `TakenSupplementsPerGuidance`
- Cash transfer / support: `BeneficiaryCashTransfer`, `ReceivingCashTransfer`, `ReceivingSupport`, `ConditionalFoodItems`
- Housing / sanitation / WASH: `HasCleanWater`, `HasHandwashingFacility`, `HasKitchenGarden`, `HasToilets`, `InsecticideTreatedBednets`
- Feeding programme indicators: `AppropriateComplementaryFeeding`, `BreastfedForSixMonths`, `FiveFoodGroups`, `MealsAtRecommendedTimes`
- Already in `nutrition-gaps.md` low-confidence: `ChildBehindOnVaccination`, `ChildReceivesECD`

### Structural enums (not clinical concepts)

| Enum | Why deferred |
|---|---|
| `VaccineDose` (First/Second/Third/Fourth/Fifth) | Dose-sequence index — captured per-vaccine, not a standalone clinical concept |
| `AdministrationNote` (administered today / previously / non-administration reasons) | Encounter-structural — describes how the dose was recorded, not what was observed |
| `ReceiveOption` (Receive / NotReceive / NotApplicable) | Generic answer enum for `receivesVitaminA`; concept is captured by `EH-PRE-051` Vitamin A |
| `StuntingLevel` (Green / Yellow / Red) | Programmatic colour-coded indicator, not a clinical fact (already in `nutrition-gaps.md:51`) |

### Low-confidence candidate flagged for reviewer

| `NCDASign` | Candidate PIH match | Why excluded for now |
|---|---|---|
| `TreatedForAcuteMalnutrition` | `PIH:20729` *Acute malnutrition* (Diagnosis/Coded) or `PIH:2234` *Malnutrition program* | E-Heza captures *receipt of treatment*, PIH options are diagnosis-presence or program enrollment — neither is a clean SAME-AS |
