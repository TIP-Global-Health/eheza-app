# Nutrition concepts without a suitable PIH match

Nutrition concepts captured by E-Heza (individual Nutrition Home Visits and
group Session encounters) that did not resolve cleanly to a PIH concept, and
so are **not** in `nutrition-concepts.csv` / `nutrition-mappings.csv`.

## Clinical signs — no PIH equivalent found

| Concept | E-Heza field | Notes |
|---|---|---|
| Apathy | `ChildNutritionSign.Apathy` | PIH:14703 Lethargy is clinically distinct |
| Brittle hair | `ChildNutritionSign.BrittleHair` | No malnutrition-related hair-change concept in PIH |
| Insufficient / lack of breast milk | `ContributingFactorsSign.FactorLackOfBreastMilk` | PIH:14061 *Sufficient milk production* is the inverse; no direct match |
| Poor suck / suckling difficulty | `ContributingFactorsSign.FactorPoorSuck` | No matching concept in PIH |
| Spermicide (contraceptive) | `FamilyPlanningSign.Spermicide` | No PIH concept for spermicide method |

## Low-confidence matches — excluded from upload

| E-Heza concept | Candidate PIH match | Why excluded |
|---|---|---|
| Moderate underweight | `PIH:20017` Underweight | No severity-specific child concept — would collide with the severe variant |
| Severe underweight | `PIH:20017` Underweight | Same — both would map SAME-AS to the same parent, which is semantically invalid |
| Child behind on vaccination | `PIH:13100` Vaccination Complete | PIH concept is the inverse (Complete vs Behind) — SAME-AS would be wrong |
| Exclusive breastfeeding for 6 months | — | PIH:13643 *Duration of breastfeeding* is only numeric; no "exclusive 6 months" flag |
| Early childhood development participation | `NCDASign.ChildReceivesECD` | PIH:11922 *Evaluate psychomotor development* differs semantically |

## Already represented in `TIP/EHEZA` — not re-created

The pilot deliberately skips concepts that are already on OCL from the
Prenatal pass, to keep each clinical fact represented by exactly one
`TIP/EHEZA` concept.

| E-Heza field | Reuses concept |
|---|---|
| `NutritionMuac` / `Muac` (group) | `EH-PRE-008` Mid-upper arm circumference (cm) |
| `NutritionHeight` / `Height` (group) | `EH-PRE-006` Height (cm) |
| `ContributingFactorsSign.FactorMaternalMastitis` | `EH-PRE-035` Postpartum mastitis |
| `FamilyPlanning` (question container) | `EH-PRE-067` Contraceptive use |

## Out of scope for this pilot (programmatic / structural, not clinical)

These E-Heza constructs are captured but were **intentionally skipped**
because they describe programs, supply chains, social determinants, or
encounter structure — not facts PIH is likely to represent, and not the
right thing to force-fit a SAME-AS on:

- **Home-visit activity signs**: most `NutritionFeedingSign` (receive supplement, ration present, sachets per day, supplement shared, encouraged to eat, clean water available, eaten with water), `NutritionHygieneSign` (soap in house, wash hands before feeding, food covered), `NutritionFoodSecuritySign` (household got food), `NutritionCaringSign` (parents alive, child clean)
- **Social determinants**: `MainWaterSource` (piped / public tap / rainwater / etc.), `WaterPreparationOption` (boiled / filtered / etc.), `MainIncomeSource`, `CaringOption`
- **Supply options**: `NutritionSupplementType` (FortifiedPorridge / RUTF / Ongera / TherapeuticMilk), `DistributionNotice`
- **Program participation**: `NCDASign` programmatic items — `AppropriateComplementaryFeeding`, `BeneficiaryCashTransfer`, `ChildReceivesFBF`, `ChildTakingFBF`, `ChildReceivesVitaminA`, `ConditionalFoodItems`, `FiveFoodGroups`, `HasCleanWater`, `HasHandwashingFacility`, `HasKitchenGarden`, `HasToilets`, `InsecticideTreatedBednets`, `MealsAtRecommendedTimes`, `OngeraMNP`, `ReceivingCashTransfer`, `ReceivingSupport`, `SupplementsDuringPregnancy`, `TakenSupplementsPerGuidance`, `TakingOngeraMNP`, `TreatedForAcuteMalnutrition`
- **Scales and levels**: `StuntingLevel` (Green/Yellow/Red), `ReceiveOption`
- **Encounter structure**: `Attendance`, `ParticipantConsent`, `CounselingSession` + `CounselingTiming` + topics, `Fbf` distribution, `FollowUpOption` time periods, `NutritionAssessment.AssesmentConsecutiveWeightLoss` / `.AssesmentDangerSignsPresent` / `.AssesmentMalnutritionSigns` (these are derived flags, not captured observations)
- **Photos** and free-text: `NutritionPhoto`, `HealthEducationValue`, `GroupSendToHC`, `NutritionSendToHC`

These can be reconsidered in a later phase if TIP decides to publish its
program-specific concepts as new TIP-local entries (not SAME-AS to PIH).
