---
name: live-queries-must-filter-field-deleted
description: "E-Heza soft-deletes with field_deleted, not node.status — a live damage query that filters on status counts already-cleaned rows"
metadata:
  node_type: memory
  type: reference
---

**In E-Heza, `node.status` is not the liveness test — `field_deleted` is.** Any live query asking
"how much damage is left" must filter `COALESCE(d.field_deleted_value, 0) = 0` by joining
`field_data_field_deleted`.

**Why it matters:** cleanup tooling marks rows with `field_deleted = TRUE` and leaves them
published. On 2026-08-27 a B-195 query filtered on `status` and reported 67 duplicate-measurement
groups / 178 nodes as live damage; with the correct filter the answer was **zero** — every one had
already been swept. The same query at encounter and participant level was unaffected, so the error
showed up only where cleanup tooling actually runs.

B-246 is a live bug of exactly this shape: the Completion report filters on publication status
instead of the soft-delete flag, so encounters an administrator deleted still count.

## Also worth knowing

Two `hedley_admin/scripts` **run daily from Jenkins** (user, 2026-08-27):
`delete-duplicate-measurements.php` (groups measurements by encounter + bundle, keeps the highest
`vid`, marks the rest `field_deleted`) and `delete-duplicates.php` (groups by **shared UUID**, keeps
the first, hard-deletes the rest — and for `person` also deletes everything referencing it).
So before filing duplicate-shaped damage, check whether one of these already owns that level.

Related: [[verify-by-running-not-reasoning]], [[prod-advancedqueue-jenkins-serialized]]
