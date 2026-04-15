# Home Visit concepts — coverage map

Home Visit (`Backend/HomeVisitActivity/Model.elm`) has four activities —
**Feeding**, **Hygiene**, **FoodSecurity**, **Caring** — and their measurement
types are reused from the Nutrition type module (`NutritionFeeding`,
`NutritionHygiene`, `NutritionFoodSecurity`, `NutritionCaring` in
`Backend/Measurement/Model.elm:393-504`). Most of these constructs are
programmatic / social-determinant facts that PIH's dictionary doesn't cover;
this file documents what landed where so reviewers can see the full picture.

## New in this pass — `homevisit-concepts.csv`

| EH id | Concept | E-Heza field | PIH | Confidence |
|---|---|---|---|---|
| `EH-HV-001` | Supplement received | `NutritionFeedingSign.ReceiveSupplement` | `PIH:10571` (Question/Coded, *"Supplement received by patient"*) | high |

## Already represented in `TIP/EHEZA` — not re-created

| `HomeVisitActivity` field | Reuses concept | Notes |
|---|---|---|
| `NutritionFeedingSign.FeedingSignBreastfeeding` | `EH-NUT-017` Breastfeeding child | Already listed in `EH-NUT-017`'s `eheza_field_path` during the Nutrition pass |
| `NutritionFeedingSign.RefusingToEat` | `EH-NUT-005` Decreased appetite | Reviewer call: behavioural refusal vs. physiological anorexia are adjacent but not identical — defensible as reuse since PIH has no distinct *"refusing to eat"* concept |

## Low-confidence candidates — deferred for reviewer attention

| E-Heza field | Candidate PIH match | Why excluded |
|---|---|---|
| `NutritionFoodSecuritySign.HouseholdGotFood` | `PIH:7411` *Food insecurity* (Diagnosis/Coded) | Inverse polarity — household *got* food ≠ food insecurity diagnosis. A SAME-AS would flip the clinical meaning. |
| `NutritionCaringSign.ParentsAliveHealthy` | `PIH:20363` *Mother alive* (Question/Coded) | PIH only has a mother-specific concept; E-Heza's sign is a joint "parents alive and healthy" check. Mapping loses the paternal side and the "healthy" qualifier. |
| `NutritionSupplementType.Rutf` | `PIH:12472` *Plumpy'Nut* / `PIH:9533` *Nourimanba (RUTF)* | Brand-specific concepts, not a generic RUTF. Already recorded in `nutrition-gaps.md:49` from the RUTF recheck (2026-04-15). |
| `NutritionFeedingValue.sachetsPerDay` | — | Free-float count of supplement sachets consumed; PIH has `PIH:20482` *Pediatric nutritional supplement* (Drug) but no "sachets per day" dose concept. |

## Confirmed gaps — no PIH equivalent found (2026-04-15 probe)

Searches run on `https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>`
— logging the negative results so the next encounter pass doesn't re-search
the same terms.

### WASH (water, sanitation, hygiene) — no PIH coverage

- **`MainWaterSource`** (6 options: `PipedWaterToHome`, `PublicWaterTap`, `RainWaterCollectionSystem`, `NaturalSourceFlowingWater`, `NaturalSourceStandingWater`, `BottledWater`). PIH's closest matches on `water+source` / `piped+water` / `surface+water` / `rainwater` are all pharmaceutical water concepts (injectable water, dextrose-in-water) or unrelated hits.
- **`WaterPreparationOption`** (5 options: `Boiled`, `PurificationSolution`, `Filtered`, `Bottled`, `NoWaterPreparationOption`). `boiled+water` / `water+treatment` / `water+filter` — same pharmaceutical hits; no water-prep concept exists.
- **`NutritionHygieneSign.SoapInTheHouse`** / **`WashHandsBeforeFeeding`** / **`FoodCovered`**. `soap` and `handwashing` return zero results; `sanitation` returns `PIH:12728` *Sanitation/hygiene counseling* (a counselling-topic concept, not a household-observation equivalent) and `PIH:12221` *Household's sanitation facility not improved (SDG)* which covers toilets, not soap.

### Programme / structural feeding signs — no PIH coverage

- **`NutritionFeedingSign`**: `ReceiveSupplement` is the only member with a clean match (now `EH-HV-001`). `RationPresentAtHome`, `EnoughTillNextSession`, `SupplementShared`, `EncouragedToEat`, `CleanWaterAvailable`, `EatenWithWater` — programme-operational observations, no PIH equivalents.
- **`NutritionSupplementType`**: `FortifiedPorridge`, `Ongera`, `TherapeuticMilk` — already gapped in `nutrition-gaps.md:49`. PIH has no generic fortified-food or MNP concept; the only ready-to-use therapeutic food matches are brand-specific (Plumpy'Nut / Nourimanba).

### Social determinants — no PIH coverage

- **`MainIncomeSource`** (4 options: `HomeBasedAgriculture`, `CommercialAgriculture`, `PublicEmployee`, `PrivateBusinessEmpployee`). PIH has the **question** concept `PIH:1304` *Main activity* (Question/Coded) and `PIH:2452` *Main activity, non-coded* (Question/Text), but no specific coded answers matching E-Heza's four categories. A SAME-AS from the whole enum to the question would lose the answer granularity; deferred.
- **`CaringOption`** (6 options: `CaredByParent`, `CaredByGrandparent`, `CaredBySibling`, `CaredByNeighbor`, `CaredByHouseHelper`, `CaredByDaycare`). No equivalent "primary caregiver identity" answer set in PIH — searches for `caregiver` / `primary+caregiver` return empty or unrelated.
- **`NutritionCaringSign.ChildClean`**. No child-cleanliness observation concept in PIH (`child+hygiene` returns counselling topics only).

## Structural items (not clinical concepts)

| Field | Why deferred |
|---|---|
| `NutritionFeedingValue.sachetsPerDay` | Free-float count; no corresponding dosage concept, and the supplement brand would need to be captured separately (see RUTF gap) |
| All `No*Signs` constructors (`NoNutritionFeedingSigns`, `NoNutritionHygieneSigns`, `NoNutritionFoodSecuritySigns`, `NoCaringSigns`, `NoNutritionSupplementType`, `NoWaterPreparationOption`) | Null-marker enum members, not clinical facts |
