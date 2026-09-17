---
name: server-report-counts-diagnoses-once
description: Statistical Queries counts how many DIFFERENT diagnoses a pregnancy received — so phase/state variants collapse to one code and `-continued` reconfirmations get NO code at all
metadata:
  type: project
---

⛔ **The server admin app's diagnosis tables answer "how many different diagnoses were made",
not "how many diagnosis records exist".** User, 2026-09-16, while scoping [[B-245]]:

- **Phase / pregnancy-state variants collapse onto ONE code** — `-initial`, `-recurrent`,
  `-immediate`, `-recheck`, `-ega-37+` describe the encounter state, not a different diagnosis.
  The map already does this: `severe-preeclampsia-{initial,recurrent,initial-ega-37+,recurrent-ega-37+}`
  all → `'d'`.
- **`-continued` / `-persistent` values get NO code and must stay OUT of the map.** They are a
  *reconfirmation* of a diagnosis an earlier encounter already recorded, so counting them again —
  under the base's code or their own — double-counts one condition in one pregnancy. Their
  derivations say so: `<base>Diagnosed && diagnosedPreviously <base>`.
- **A different diagnosis ROUTE is still the same diagnosis** — `-by-symptoms` reaches the same
  condition through the symptom review instead of the danger-signs list, under an identical gate,
  so it collapses onto the base code.

**Why:** the client needs the distinction (a continued infection changes the treatment — medication
becomes a hospital referral); the server report does not, because it counts conditions per
pregnancy. ⛔ **Do not argue from the client's progress report about what the server report should
show** — different surface, different question. I made that mistake twice on B-245.

**How to apply:** before adding a value to `hedley_reports_*_diagnosis_mapping()`, read its
derivation in `client/src/elm/Pages/*/Activity/Utils.elm`. A `diagnosedPreviously` /
`symptomRecordedPreviously` clause means omit it; no such clause means it is a first diagnosis and
needs a code. Related: [[live-queries-must-filter-field-deleted]], [[read-the-issue-for-requirements]].
