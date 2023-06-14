<?php

/**
 * @file
 * Deletes duplicate measurements from session.
 *
 * Before execution:
 *   drush vset hedley_super_user_mode 1
 *
 * Execution:
 *   drush scr profiles/hedley/modules/custom/hedley_admin/scripts/
 *             delete-duplicate-measurements.php --session=[session ID].
 *
 * After execution:
 *   drush vset hedley_super_user_mode 0
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 250);

$fields = field_info_fields();
// $session_bundles = $fields['field_session']['bundles']['node'];
$nutrition_bundles = $fields['field_nutrition_encounter']['bundles']['node'];

$total_deleted = 0;
foreach ($nutrition_bundles as $bundle) {
  $query = db_select('field_data_field_nutrition_encounter', 'ne');
  $query->fields('ne', ['field_nutrition_encounter_target_id']);
  $query->condition('ne.bundle', $bundle);
  $query->addExpression('COUNT(ne.field_nutrition_encounter_target_id)', 'total');
  $query->groupBy('ne.field_nutrition_encounter_target_id');
  $query->havingCondition('total', 1, '>');
  $query->range(0, 2000);
  $result = $query->execute()->fetchAllAssoc('field_nutrition_encounter_target_id');
  $encounters_with_duplicates = array_keys($result);

  foreach ($encounters_with_duplicates as $encounter) {
    $query = db_select('field_data_field_nutrition_encounter', 'ne');
    $query->fields('ne', ['entity_id']);
    $query->condition('ne.bundle', $bundle);
    $query->condition('ne.field_nutrition_encounter_target_id', $encounter);
    $query->orderBy('ne.entity_id', 'DESC');
    $result = $query->execute()->fetchAllAssoc('entity_id');

    $duplicates = array_keys($result);
    $first = array_shift($duplicates);
    $total_for_deletion = count($duplicates);
    $total_deleted += $total_for_deletion;
    drush_print("Deleting $total_for_deletion duplicates of $bundle bundle at encounter $encounter");
    // node_delete_multiple($duplicates);
  }
}

if (round(memory_get_usage() / 1048576) >= $memory_limit) {
  drush_print(dt('Stopped before out of memory.'));
  return;
}

drush_print("Done! Total of $total_deleted duplicate measurements deleted.");
