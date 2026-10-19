# HealthyStart dashboards: data

The Facility Performance and Program Monitoring dashboards live in the backend Elm app under
`admin/reports/dashboards/`.

This file describes their data: what is stored, how it stays current, what each figure needs, and
what is still undefined.

## Data flow

1. Every antenatal pregnancy (`individual_participant` node) holds one JSON record in
   `field_reports_data`, described below.
2. An AdvancedQueue item rewrites the record when the pregnancy, one of its prenatal encounters, or
   its newborn's routine care changes. Jenkins drains the queue every ten minutes.
3. The Elm app loads all records in batches from `/api/reports-data` with `app_type = dashboard`,
   then filters by site, location and period on the client. Nothing is precomputed per filter.

The code is in `server/hedley/modules/custom/hedley_reports/hedley_reports.dashboard.inc`.

## Which pregnancies get a record

- Antenatal participants only.
- On sites where the Healthy Start feature is enabled.
- Registered on or after the first cohort date, set at `admin/config/system/healthy-start`. While
  the date is empty, every antenatal pregnancy counts.

## The record

Dates are `YYYY-MM-DD`. A missing value is `null`.

| Key | Meaning |
| --- | --- |
| `v` | Format version, `HEDLEY_REPORTS_DASHBOARD_DATA_VERSION`. |
| `id` | Pregnancy node id. |
| `hc` | Health center node id, from the pregnancy's shard. |
| `created` | Registration date. |
| `edd` | Expected delivery date. LMP is 280 days before it. |
| `dc` | Conclusion date. |
| `o` | Outcome: `a` live at term, `b` live preterm, `c` stillbirth at term, `d` stillbirth preterm, `e` abortion. |
| `ol` | Delivery location: `f` facility, `h` home. |
| `visits` | Prenatal encounters, earliest first. |
| `child` | The newborn registered on the pregnancy, or `null`. |

Each entry of `visits`:

| Key | Meaning |
| --- | --- |
| `d` | Encounter date. |
| `t` | Encounter type: `nurse`, `nurse-postpartum`, `chw-1`, `chw-2`, `chw-3`, `chw-postpartum`. |
| `n` | Next visit date the encounter set, if any. |

The `child` object:

| Key | Meaning |
| --- | --- |
| `id` | Child person node id. |
| `bd` | Birth date. |
| `g` | Gender: `m` or `f`. |
| `visits` | Routine care encounters, earliest first: `d` date, `t` is `well-child` or `nutrition`. |

## What each headline tile needs

The nine tiles across the top of both dashboards. "Status" says whether the record already carries
the data and whether HealthyStart has defined the figure.

| Tile | Record fields | Status |
| --- | --- | --- |
| Women Currently in ANC Care | `dc`, `edd` | Data present. Cutoff for pregnancies never concluded is undefined. |
| Pregnancies 1st / 2nd / 3rd trimester | `edd`, `dc` | Data present. First trimester boundary (under 13 or under 14 weeks) is unconfirmed. |
| ANC Miss Rate | `visits.d`, `visits.n`, `edd` | Data present. Definition of a missed visit is open. |
| Total Deliveries (Institutional) | `dc`, `ol` | Data present. Facility delivery location in the selected period. |
| Total Children on Routine Care | `child.visits` | Data present. Definition is open. |
| Graduated Mother–Child Pairs | `child.bd`, `child.visits` | Data present. Definition of graduation is open. |
| Lost to Follow-Up | `visits`, `child.visits`, `dc` | Data present. Grace period is open. |

Cross-cutting questions still open: what Week, Month, Quarter and Year select exactly, and
whether a pregnancy is in the cohort by registration date or by an explicit flag.

## Not in the record yet

Needed by the indicator blocks, coverage bars, threshold tiles and alerts. Each is added to the
record, with a trigger for its content type, when its figure is built.

| Figure | Source content |
| --- | --- |
| Early ANC booking, median GA at first ANC | `visits.d` and `edd` already suffice. |
| Early ultrasound coverage | `prenatal_ultrasound` measurement date. |
| High-risk pregnancy | Diagnoses, hemoglobin, blood sugar, medical history, obstetric history, nutrition, HIV, mother's birth date. |
| Postpartum hemorrhage | Not captured by the app yet. |
| Complete ANC attendance | `visits` already suffice. |
| Aspirin and calcium coverage | `prenatal_aspirin`, `prenatal_calcium`; eligibility rule. |
| SQLNS coverage | `well_child_feeding`, `nutrition_feeding` supplement type. |
| Inadequate GWG | `field_prenatal_indicators` on the encounter. |
| Birth weight recording, SGA | `well_child_pregnancy_summary` weight and its measurement date. |
| Child growth outcomes | Z-scores on `well_child_weight` and `well_child_height`. |

## How to extend the record

1. Add the field in `hedley_reports_dashboard_pregnancy_record()` or
   `hedley_reports_dashboard_child_record()`.
2. If a new content type feeds it, resolve that type to its pregnancy in
   `hedley_reports_dashboard_pregnancies_for_node()`.
3. Bump `HEDLEY_REPORTS_DASHBOARD_DATA_VERSION`.
4. Decode the field in the Elm app, treating records of an older version as missing it.
5. Rerun the script below on every HealthyStart site, without `--exclude_set`.
6. Update the tables in this file.

## Scripts and operations

`dashboard-generate-pregnancy-data.php` in `hedley_reports/scripts` writes records in batches.
Options: `--nid`, `--batch`, `--exclude_set=1` for pregnancies without a record, `--open=1` for
unconcluded pregnancies, `--memory_limit`.

- At rollout, and after a format change: a full run.
- Nightly, in the Jenkins Overnight job: `--exclude_set=1`, then `--open=1`, to cover queue
  items that failed their three attempts.

The record is written with `field_attach_update()`, so no node revision is created and devices
do not resync the pregnancy.
