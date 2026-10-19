# HealthyStart dashboards: data

The Facility Performance and Program Monitoring dashboards live in the backend Elm app under
`admin/reports/dashboards/`.

This file describes their data: what is stored, how it stays current, what each figure needs, and
what is still undefined.

## Data flow

1. Every antenatal pregnancy (`individual_participant` node) holds one JSON record in
   `field_reports_data`, described below.
2. An AdvancedQueue item rewrites the record when the pregnancy, a prenatal encounter or a
   measurement it reads, the mother, or the newborn's data change. Jenkins drains it every ten
   minutes.
3. The Elm app loads all records in batches from `/api/reports-data` with `app_type = dashboard`,
   then filters by site, location and period on the client. Nothing is precomputed per filter.

The code is in `server/hedley/modules/custom/hedley_reports/hedley_reports.dashboard.inc`.

## Which pregnancies get a record

- Antenatal participants only, published and not marked deleted.
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
| `mbd` | Mother's birth date. |
| `edd` | Expected delivery date. LMP is 280 days before it. |
| `dc` | Conclusion date. |
| `o` | Outcome: `a` live at term, `b` live preterm, `c` stillbirth at term, `d` stillbirth preterm, `e` abortion. |
| `ol` | Delivery location: `f` facility, `h` home. |
| `visits` | Prenatal encounters, earliest first. |
| `us` | Dates of ultrasound examinations, earliest first. |
| `risks` | High-risk criteria met, see below. Empty when none. |
| `child` | The newborn registered on the pregnancy, or `null`. |

Each entry of `visits`:

| Key | Meaning |
| --- | --- |
| `d` | Encounter date. |
| `t` | Encounter type: `nurse`, `nurse-postpartum`, `chw-1`, `chw-2`, `chw-3`, `chw-postpartum`. |
| `n` | Next visit date the encounter set, if any. |
| `dx` | Codes of the diagnoses made at that visit, as `hedley_reports_prenatal_diagnosis_mapping()` assigns them. Diagnoses entered through Outside Care count for `risks` only. |
| `ind` | Indicator codes as `hedley_reports_prenatal_indicator_mapping()` assigns them: `a` adequate GWG, `b` MMS, `c` ultrasound referral, `d` aspirin, `e` calcium. |

Aspirin, calcium and MMS count when the measurement says "administered today". Aspirin also counts
when the medication distribution prescribed it.

The `risks` codes, HealthyStart's list of 18 October 2026, and what raises each:

| Code | Criterion | Raised by |
| --- | --- | --- |
| `pe` | Risk of pre-eclampsia | Diagnoses `m2` or `n2`, the ones that start low-dose aspirin. |
| `anemia` | Severe anaemia, Hb under 7 | Diagnoses `q`, `s` or `t`. |
| `diabetes` | Pre-existing diabetes, or glucose over 11 mmol/L | Diagnoses `t1` or `u1`, from the app's glucose rule. |
| `malaria` | Confirmed malaria | Diagnoses `o`, `p` or `q`. |
| `csection` | Prior caesarean section | A C-section count in obstetric history, or a scar in the obstetrical exam. |
| `ncd` | Pre-existing NCD or heart disease | Chronic hypertension diagnosed, or asthma, cardiac, renal, hypertension or diabetes in the medical history. |
| `age` | Under 18 or over 40 | Mother's age at the first visit, or at registration without visits. |
| `hiv` | Known HIV infection | Positive HIV status on the person, HIV in the medical history, a positive test, or diagnoses `h`, `i`. |
| `underweight` | BMI under 17.5 or MUAC under 21 cm | Any prenatal nutrition measurement. |

"Complex health conditions" has no field behind it and is left out, as agreed on #2231.

The `child` object:

| Key | Meaning |
| --- | --- |
| `id` | Child person node id. |
| `bd` | Birth date. |
| `g` | Gender: `m` or `f`. |
| `visits` | Routine care visits, earliest first: `d` date, `t` is `well-child`, `nutrition` or `group-nutrition` (a group session attended). |

## What each headline tile needs

The nine tiles across the top of both dashboards. "Status" says whether the record already carries
the data and whether HealthyStart has defined the figure.

| Tile | Record fields | Status |
| --- | --- | --- |
| Women Currently in ANC Care | `dc`, `edd` | Data present. Cutoff for pregnancies never concluded is undefined. |
| Pregnancies 1st / 2nd / 3rd trimester | `edd`, `dc` | Data present. The app's boundaries: first trimester under 13 weeks, second under 28. |
| ANC Miss Rate | `visits.d`, `visits.n`, `edd` | Data present. Definition of a missed visit is open. |
| Total Deliveries (Institutional) | `dc`, `ol` | Data present. Facility delivery location in the selected period. |
| Total Children on Routine Care | `child.visits` | Data present. Definition is open. |
| Graduated Mother–Child Pairs | `child.bd`, `child.visits` | Data present. Definition of graduation is open. |
| Lost to Follow-Up | `visits`, `child.visits`, `dc` | Data present. Grace period is open. |

Cross-cutting questions still open: what Week, Month, Quarter and Year select exactly, and
whether a pregnancy is in the cohort by registration date or by an explicit flag.

## What the indicator blocks and coverage bars need

The six KPI blocks of the Facility dashboard and the three coverage bars. Formulas are the spec's,
posted on #2231.

| Figure | Formula | Record fields | Status |
| --- | --- | --- | --- |
| Early ANC Booking Rate | First visit in the first trimester ÷ ANC clients | `visits.d`, `edd` | Data present. The app's first trimester, under 13 weeks, decided on 19 October 2026; the spec said under 14. Any visit type counts as first ANC. |
| Median GA at First ANC | Median gestational age at first visit | `visits.d`, `edd` | Data present. |
| Early Ultrasound Coverage | Ultrasound under 20 weeks ÷ eligible pregnancies | `us`, `edd` | Data present. "Eligible pregnancies" is undefined. |
| High-Risk Pregnancy Rate | High-risk ÷ total pregnancies | `risks` | Data present. The NCD list and the C-section sources above are our reading; to confirm. |
| Postpartum Hemorrhage Rate | PPH cases ÷ deliveries | none | Not captured by the app. Parked by HealthyStart. |
| Complete ANC Attendance Rate | 8 or more visits ÷ eligible women | `visits` | Data present. "Eligible women" is undefined; likely concluded pregnancies. |
| Aspirin coverage | Received aspirin ÷ eligible women | `ind` code `d`, `risks` code `pe` | Data present. Eligible means the pre-eclampsia risk, as proposed on #2235. |
| Calcium coverage | Received calcium ÷ women in ANC care | `ind` code `e` | Data present. |
| SQLNS coverage | Children receiving SQLNS ÷ children on routine care | none | Later, with the nutrition figures. |

Each block also shows the program target and the inter-site average. The inter-site average is
the mean over the other sites' records. Targets have no source yet; placeholders meanwhile.

## How the Facility dashboard counts

`Pages/Dashboard/Facility.elm` works the figures out from the selected site's records. The Program
dashboard still shows placeholder figures, and so do the Facility critical alerts.

The Time filter picks the period holding today, up to today. Weeks count from the first cohort
date. Each block compares with the whole previous period of the same kind.

Gestational age counts from the LMP, 280 days before the EDD; figures that need it skip pregnancies
without one. Antenatal visits are every visit type but postpartum.

| Figure | Pregnancies counted | Of which |
| --- | --- | --- |
| Women Currently in ANC Care | Not concluded, under 42 weeks today | |
| Pregnancies by trimester | The same, by gestational age today | |
| Total Deliveries (Institutional) | Concluded in the period, delivered at a facility | |
| Total Children on Routine Care | Newborn has a routine care visit in the period | |
| Early ANC Booking Rate | First antenatal visit in the period | First visit under 13 weeks |
| Median GA at First ANC | First antenatal visit in the period | Median weeks at that visit |
| Early Ultrasound Coverage | Reached 20 weeks in the period, seen by then | Scanned before 20 weeks |
| High-Risk Pregnancy Rate | An antenatal visit in the period | Any `risks` code |
| Complete ANC Attendance Rate | Concluded in the period | 8 or more antenatal visits |
| Aspirin coverage | An antenatal visit in the period, `pe` risk | Aspirin at any visit |
| Calcium coverage | An antenatal visit in the period | Calcium at any visit |

A dash shows a rate with no pregnancy to count, and the figures not defined or captured yet: ANC
Miss Rate, Graduated Pairs, Lost to Follow-Up, PPH, SQLNS.

The trend chart and the drill down count each calendar month the same way. Months still to come
stay empty. Targets are placeholders.

## Not in the record yet

Needed by the threshold tiles and alerts. Each is added to the record, with a trigger for its
content type, when its figure is built.

| Figure | Source content |
| --- | --- |
| SQLNS coverage | `well_child_feeding`, `nutrition_feeding` supplement type. |
| Birth weight recording, SGA | `well_child_pregnancy_summary` weight and its measurement date. |
| Child growth outcomes | Z-scores on `well_child_weight` and `well_child_height`. |

## How to extend the record

1. Add the field in `hedley_reports_dashboard_pregnancy_record()` or
   `hedley_reports_dashboard_child_record()`.
2. If a new content type feeds it, resolve that type to its pregnancy in
   `hedley_reports_dashboard_pregnancies_for_node()`. A prenatal measurement already resolves
   through its encounter; add its bundle to `hedley_reports_dashboard_measurement_bundles()`.
3. Bump `HEDLEY_REPORTS_DASHBOARD_DATA_VERSION`.
4. Decode the field in `Backend/Dashboard/Decoder.elm`, treating records of an older version as missing it.
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
