# Patient Registration → OCL/PIH concept mapping — design

Adds a Patient Registration mapping group to `docs/ocl/`, parallel to the
existing nine per-encounter groups (Prenatal, Nutrition, Well-Child, Acute
Illness, NCD, HIV, Tuberculosis, Child Scoreboard, Home Visit). Source of
truth is the `Person` record in `client/src/elm/Backend/Person/Model.elm`
— the demographic / contact / socioeconomic fields a nurse fills at first
contact and that feed every subsequent encounter.

ID prefix: **`EH-PER`**. Deliverable scope: **artefacts only** (CSVs,
gaps doc, README updates, translations). Upload to OCL is explicitly
out of scope for this work — that step is run separately per the README's
existing `delta_upload.py` workflow.

## Scope

In: every substantive field on the `Person` record except the three
system internals (`healthCenterId`, `deleted`, `shard`). The computed
`name` field (built from `firstName + secondName` via `generateFullName`)
is excluded as derived. That leaves **27 candidate fields**.

| Group | Fields |
|---|---|
| Names | `firstName`, `secondName` |
| Identifiers | `nationalIdNumber`, `hmisNumber`, `avatarUrl` |
| Demographics | `birthDate`, `isDateOfBirthEstimated`, `gender` |
| Coded clinical / socioeconomic | `hivStatus`, `numberOfChildren`, `modeOfDelivery`, `ubudehe`, `educationLevel`, `maritalStatus` |
| Administrative geography | `province`, `district`, `sector`, `cell`, `village` |
| GPS | `registrationLatitude`, `registrationLongitude`, `saveGPSLocation` |
| Contact | `telephoneNumber` |
| Relationships | `spouseName`, `spousePhoneNumber`, `nextOfKinName`, `nextOfKinPhoneNumber` |

Coded enum fields collapse to a single `Question/Coded` concept each
(e.g., one "Marital status" concept, not one per `Married`/`Single`/…).
The answer set lives on the PIH side of the SAME-AS mapping. This
matches the existing pattern set by `EH-NCD-049` "History of alcohol use"
and `EH-NUT-017` "Breastfeeding child".

## Per-concept row plan

Numbering reserves `EH-PER-001..027` in the order below; final assignment
happens during implementation so consecutive ids land in adjacent rows.
Each row's class/datatype is the *target* — actual values get aligned to
the matched PIH concept's class/datatype before commit (per the existing
README §Methodology step 3).

| EH | Field | Proposed name | Class | Datatype | PIH probe terms |
|---|---|---|---|---|---|
| 001 | `firstName` | First name | Misc / Question | Text | `first+name`, `given+name` |
| 002 | `secondName` | Family name | Misc / Question | Text | `family+name`, `surname`, `last+name` |
| 003 | `nationalIdNumber` | National ID number | Misc | Text | `national+id`, `national+identification` |
| 004 | `hmisNumber` | HMIS number | Misc | Text | `hmis`, `health+management+information` |
| 005 | `avatarUrl` | Patient photo | Misc | Text | `patient+photo`, `photograph` |
| 006 | `birthDate` | Date of birth | Misc | Date | `date+of+birth`, `birthdate` |
| 007 | `isDateOfBirthEstimated` | Birth date estimated | Question | Boolean | `estimated+date+of+birth`, `birthdate+estimated` |
| 008 | `gender` | Gender | Question | Coded | `gender`, `sex` |
| 009 | `hivStatus` | HIV status | Question | Coded | `hiv+status`, `hiv+result` |
| 010 | `numberOfChildren` | Number of children under 5 | Question | Numeric | `number+of+children`, `living+children` |
| 011 | `modeOfDelivery` | Mode of delivery | Question | Coded | `mode+of+delivery`, `delivery+method` |
| 012 | `ubudehe` | Ubudehe socioeconomic category | Question | Coded | `ubudehe`, `socioeconomic+category` |
| 013 | `educationLevel` | Highest level of education | Question | Coded | `level+of+education`, `highest+education` |
| 014 | `maritalStatus` | Marital status | Question | Coded | `marital+status`, `civil+status` |
| 015 | `province` | Province | Misc | Text | `province` |
| 016 | `district` | District | Misc | Text | `district` |
| 017 | `sector` | Sector | Misc | Text | `sector`, `subdivision` |
| 018 | `cell` | Cell | Misc | Text | `cell`, `village+cell` |
| 019 | `village` | Village | Misc | Text | `village`, `umudugudu` |
| 020 | `registrationLatitude` | GPS latitude | Misc | Numeric | `latitude`, `gps+coordinates` |
| 021 | `registrationLongitude` | GPS longitude | Misc | Numeric | `longitude`, `gps+coordinates` |
| 022 | `saveGPSLocation` | GPS location consent | Question | Boolean | `gps+consent`, `location+consent` |
| 023 | `telephoneNumber` | Telephone number | Misc | Text | `telephone`, `phone+number` |
| 024 | `spouseName` | Spouse name | Misc | Text | `spouse+name`, `partner+name` |
| 025 | `spousePhoneNumber` | Spouse phone number | Misc | Text | `spouse+phone`, `partner+phone` |
| 026 | `nextOfKinName` | Next of kin name | Misc | Text | `next+of+kin+name`, `emergency+contact` |
| 027 | `nextOfKinPhoneNumber` | Next of kin phone number | Misc | Text | `next+of+kin+phone`, `emergency+contact+phone` |

Probes that don't resolve (or resolve only with semantic drift) move
to `patient-registration-gaps.md` instead of the CSV. Likely gap
candidates ahead of probing: `ubudehe` (Rwanda-specific), `hmisNumber`
(program-specific), `avatarUrl`, `saveGPSLocation` (operational toggle),
admin geography (`province`/`district`/`sector`/`cell`/`village`),
`isDateOfBirthEstimated` (metadata flag).

### Reuse rule

Per the README's "reuse over duplication" rule: before minting any
`EH-PER-NNN`, scan existing `*-mappings.csv` for the same
`to_concept_code`. Hits go into the gaps file's *Already represented*
section instead, and the existing concept's `eheza_field_path` is
extended to include the new Person-record reference. Most likely hit:
`numberOfChildren` may collapse onto `EH-PRE-019` "Total number of
living children".

### Confidence column

- `high` — exact name match + correct class + correct datatype
- `medium` — clinically equivalent but minor drift (naming, class, representation)
- `low` — plausible match, reviewer should confirm; also surfaced in gaps file

## Convention extensions (README updates)

The Person record lives outside the per-encounter pattern, so a few
README facts need extending.

1. **`eheza_field_path` source extension** — README §Concept CSV columns
   currently says the column points at
   `client/src/elm/Backend/Measurement/Model.elm`. Add one line: *"For
   patient-registration rows (`EH-PER-*`), this column points at the
   field in `client/src/elm/Backend/Person/Model.elm` instead."*
2. **Coverage table row** — add a new row in §What's here for Patient
   registration with `EH-PER` prefix and the eventual concept / mapping
   counts. Bump the totals row.
3. **§Scope of this pass** — add a sentence noting that patient
   registration is not an encounter type but a person-level field set,
   captured because it's the data nurses enter at first contact and
   feeds every subsequent encounter.
4. **§Methodology** — add a brief variant for the registration pass:
   inventory walks the `Person` record fields rather than encounter
   measurement aliases; otherwise steps 2–4 (naming, PIH matching,
   confidence scoring) are unchanged.
5. **§Where the Elm code lives** — add
   `client/src/elm/Backend/Person/Model.elm` to the bullet list with a
   short description.

No changes needed to `csv_to_ocl_json.py` or `delta_upload.py` — both
glob `*-concepts.csv` / `*-mappings.csv`, so the new pair is picked up
automatically.

## `patient-registration-gaps.md` shape

Same shape as `homevisit-gaps.md` and `nutrition-gaps.md`. Sections that
end up empty are omitted at write time.

- **Header paragraph** — one short paragraph: this file documents
  Person-record fields evaluated for OCL publication, why each was
  excluded, and which were already represented elsewhere in `TIP/EHEZA`.
- **§ New in this pass — `patient-registration-concepts.csv`** —
  compact table (EH id / concept / E-Heza field / PIH / confidence)
  for every concept that did land in the CSV.
- **§ Already represented in `TIP/EHEZA` — not re-created** — rows
  for fields whose match collapses onto an existing `EH-` id (e.g.,
  `numberOfChildren` → `EH-PRE-019`).
- **§ Low-confidence candidates — deferred for reviewer attention**
  — rows where a PIH match exists but is semantically off (wrong
  polarity, wrong granularity, brand-specific, …).
- **§ Confirmed gaps — no PIH equivalent found (YYYY-MM-DD probe)**
  — log the probe terms used so the next pass doesn't re-search them.
  Grouped: Personal identifiers / Administrative geography /
  Rwanda-specific / Operational toggles / Metadata flags.

## Translations

Per the README's existing locale policy, populate non-English names
only where `Translate.elm` carries `Just _` values — never auto-fill
with the English string (CLAUDE.md is explicit on the runtime fallback).
Translation IDs to inspect: `Translate.FirstName`, `SecondName`,
`NationalIdNumber`, `ChildHmisNumber`, `Photo`, `BirthDate`,
`Gender`, `HIVStatusLabel`, `NumberOfChildrenUnder5`,
`ModeOfDeliveryLabel`, `Ubudehe`, `LevelOfEducationLabel`,
`MaritalStatusLabel`, `Province`, `District`, `Sector`, `Cell`,
`Village`, `Latitude`, `Longitude`, `TelephoneNumber`,
plus any that surface during step 1 below.

Extracted entries get appended to `docs/ocl/translations.jsonl` keyed
by the new `EH-PER-NNN` ids.

## Implementation sequence

Discrete steps, each independently reviewable. Per CLAUDE.md, every
commit ends with `[ci skip]`.

1. **Probe** — Run the OCL search API for each of the 27 candidates
   (`https://api.openconceptlab.org/orgs/PIH/sources/PIH/concepts/?q=<term>&limit=10`),
   capture the best hits and a confidence score per the rubric above.
   Output is a working table; nothing committed yet.
2. **Reuse check** — For each match, grep existing
   `docs/ocl/*-mappings.csv` for the same `to_concept_code`. Hits move
   to the gaps file's *Already represented* section.
3. **Translation extraction** — Read the relevant `Translate.elm` IDs
   and extract `Just _` rw/rn/so values. Build the corresponding
   `translations.jsonl` entries.
4. **Write artefacts** — Create
   `docs/ocl/patient-registration-concepts.csv`,
   `docs/ocl/patient-registration-mappings.csv`, and
   `docs/ocl/patient-registration-gaps.md`. Update
   `docs/ocl/README.md` per *Convention extensions* above. Append to
   `docs/ocl/translations.jsonl`.
5. **Local validation**:
   - Row count parity:
     ```bash
     for f in docs/ocl/patient-registration-concepts.csv; do
       base="${f%-concepts.csv}"
       c=$(($(wc -l < "$base-concepts.csv") - 1))
       m=$(($(wc -l < "$base-mappings.csv") - 1))
       printf "%-30s %3d concepts / %3d mappings\n" "$(basename $base)" "$c" "$m"
     done
     ```
   - Spot-check 3–5 of the new `to_concept_code` values resolve via the
     curl one-liner in README §Verification.
6. **Commit** — one commit per logical step (CSVs, gaps doc, README,
   translations.jsonl). Each ends with `[ci skip]`.

Upload (`delta_upload.py` → bulk-import → cut released source version)
is **out of scope** for this work and runs separately when reviewers
sign off.
