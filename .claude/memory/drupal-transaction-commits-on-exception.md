---
name: drupal-transaction-commits-on-exception
description: ⛔ a D7 db_transaction() that is never rolled back COMMITS when the script dies — wrap rollback tests in try/finally; hit 2026-10-18 on the local live copy
metadata:
  node_type: memory
  type: feedback
  originSessionId: 27f0a64b-384c-4380-a186-28a0599a31b2
  modified: 2026-10-18T09:29:20.872Z
---

A "rolled-back" test harness (`$txn = db_transaction(); ...; $txn->rollback();`) is only safe if
the rollback always runs. In Drupal 7, when an exception aborts the script, the transaction object
is destroyed and its work is **committed**, not discarded.

**Why:** 2026-10-18, testing PR #2373's incident script against the local Rwanda live copy. The
harness's own setup wrote test data, then called `cache_clear_all('*', 'cache_entity_user')` on a
table that does not exist; the exception committed the setup. It deleted 4 users'
`field_incident_details` rows, rewrote user 4411's, and renamed a person's UUID. The UUID was
restorable; the detail rows hold patient names, and writing them back was blocked by the auto-mode
classifier as PII handling, so the user had to decide.

**How to apply:**
- Put every write AND the code under test inside `try { ... } finally { $txn->rollback(); }`.
- Before any write, capture what the test will change (row counts, checksums) so damage is
  measurable, and prefer a `ddev snapshot` of the local DB first when the copy is not reproducible.
- Keep setup writes minimal; never delete rows a restore would need patient data to recreate.

Related: [[verify-by-running-not-reasoning]], [[ddev-from-worktree-starts-second-project]].
