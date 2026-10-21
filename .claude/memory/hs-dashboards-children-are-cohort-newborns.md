---
name: hs-dashboards-children-are-cohort-newborns
description: "HealthyStart dashboards count only the newborns of cohort pregnancies (linked via field_newborn), not every child the facility sees; asked Adam to confirm on #2231 (2026-10-20)"
metadata:
  node_type: memory
  type: project
  originSessionId: 6c76f54a-e619-429c-8cec-89e224769728
  modified: 2026-10-20T10:23:25.025Z
---

On the HealthyStart dashboards (#2230–#2237), every child figure (Total Children on Routine
Care, Graduated Mother–Child Pairs, the child leg of Lost to Follow-Up, SQLNS coverage,
Poor Child Growth Outcomes, birth weight tiles and alerts) counts the **newborns of the
pregnancies in the cohort**, i.e. the child registered on the pregnancy at its conclusion
(`field_newborn`). A child who reaches the health centre by any other route is not counted.

The pregnancy record in `field_reports_data` (PR #2379, `docs/healthy-start-dashboards.md`)
embeds the child under the pregnancy for this reason.

Status: our reading, put to Adam on #2231
(https://github.com/TIP-Global-Health/eheza-app/issues/2231#issuecomment-6288661137,
2026-10-20) together with the open child definitions (routine care, graduation, child LTFU
grace period, Slow vs Inadequate Growth, SQLNS source and denominator). Not yet confirmed.

**Why:** the program is a mother–child continuum ("Graduated Mother–Child Pairs",
"Lost to Follow-Up (ANC–PNC–Child)"); Adam framed it as "an entirely new population set
following a pre-set clinical path" (#2231, 2026-09-15). No cohort setting exists for children,
and a creation-date cut-off would be arbitrary.

**How to apply:** do not build child figures keyed by child creation date or by every child
with a visit. Risk: newborns registered separately and never linked to the pregnancy are
invisible; if that happens in practice, add a fallback through the mother–child relationship
nodes (children born within weeks of the conclusion date), only after Adam confirms.
Related: [[feature-healthy-start]].
