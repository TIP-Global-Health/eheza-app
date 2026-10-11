---
name: exclude-deleted-helper-single-alias
description: ⛔ hedley_general_apply_exclude_deleted() can only be called ONCE per query — it hardcodes the alias `fd`, so a second call silently leaves that node unfiltered
metadata:
  type: reference
---

⛔ **`hedley_general_apply_exclude_deleted($query, $node_alias)` is single-use per query**
(`server/hedley/modules/custom/hedley_general/hedley_general.module`):

```php
function hedley_general_apply_exclude_deleted($query, $node_alias = 'n') {
  $query->leftJoin('field_data_field_deleted', 'fd', $node_alias . '.nid = fd.entity_id');
  $or = db_or()
    ->isNull('fd.field_deleted_value')
    ->condition('fd.field_deleted_value', 0);
  $query->condition($or);
  return $query;
}
```

The alias `fd` is hardcoded in **both** the ON clause and the WHERE conditions. Call it twice and
Drupal renames the second join to `fd_2`, but its ON clause still references `fd`, so the second
node is never filtered and the first node's condition is simply duplicated. The query still returns
plausible rows, which is why this passes review by eye.

`hedley_general_create_db_select_query_excluding_deleted()` calls it, so that counts as the one use.

**How to apply:** joining more than one node type in a query that must skip deleted rows — encounter
*and* participant *and* person, say — means joining `field_data_field_deleted` by hand, once per node
alias, each with its own alias. Done this way in
`hedley_reports/scripts/recalculate-newly-mapped-prenatal-diagnoses.php`. ⭐ Verify the result by
running the equivalent raw SQL through [[production-sql-via-host-terminus]] and comparing counts —
that is what caught it.
