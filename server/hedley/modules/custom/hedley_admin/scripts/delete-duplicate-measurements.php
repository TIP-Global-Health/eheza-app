<?php

/**
 * @file
 * Deletes duplicate measurements of same type associated with an encounter.
 *
 * Covers group sessions and all types of individual encounters.
 *
 * Before execution:
 *   drush vset hedley_super_user_mode 1
 *
 * Execution:
 *   drush scr
 *     profiles/hedley/modules/custom/hedley_admin/scripts/delete-duplicate-measurements.php
 *
 * After execution:
 *   drush vset hedley_super_user_mode 0
 */

if (!drupal_is_cli()) {
  // Prevent execution from the browser.
  return;
}

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$fields = field_info_fields();
$encounter_types = hedley_general_get_encounter_types();

$total_deleted = 0;

foreach ($encounter_types as $encounter_type) {
  drush_print("Deleting duplicates for $encounter_type encounter...");
  $deleted_for_encounter = 0;
  $bundles = $fields["field_$encounter_type"]['bundles']['node'];
  foreach ($bundles as $bundle) {
    $query = db_select("field_data_field_$encounter_type", 'et');
    $query->addField('et', "field_{$encounter_type}_target_id");
    if ($encounter_type == 'session') {
      $query->leftJoin('field_data_field_person', 'fp', 'fp.entity_id = et.entity_id');
      $query->addField('fp', 'field_person_target_id');
    }
    $query->condition('et.bundle', $bundle);
    $query->addExpression("COUNT(et.field_{$encounter_type}_target_id)", 'total');
    $query->groupBy("et.field_{$encounter_type}_target_id");
    if ($encounter_type == 'session') {
      $query->groupBy("fp.field_person_target_id");
    }
    $query->havingCondition('total', 1, '>');
    $query->range(0, 2000);
    $duplicates_data = $query->execute()->fetchAll();

    foreach ($duplicates_data as $data) {
      $encounter = $data->{"field_{$encounter_type}_target_id"};
      $query = db_select("field_data_field_$encounter_type", 'et');
      $query->leftJoin('node', 'n', 'n.nid = et.entity_id');
      $query->addField('et', 'entity_id');
      if ($encounter_type == 'session') {
        $person_id = $data->field_person_target_id;
        if (!$person_id) {
          // Can not resolve duplicates due to failure to retrieve person ID.
          continue;
        }
        $query->leftJoin('field_data_field_person', 'fp', 'fp.entity_id = et.entity_id');
        $query->addField('fp', 'field_person_target_id');
        $query->condition('fp.field_person_target_id', $person_id);
      }
      $query->condition('et.bundle', $bundle);
      $query->condition("et.field_{$encounter_type}_target_id", $encounter);
      $query->orderBy('n.vid', 'DESC');
      $result = $query->execute()->fetchAllAssoc('entity_id');

      $duplicates = array_keys($result);
      $duplicate_nodes = node_load_multiple($duplicates);
      $undeleted_found = FALSE;
      $total_for_deletion = 0;
      foreach ($duplicate_nodes as $duplicate_node) {
        $wrapper = entity_metadata_wrapper('node', $duplicate_node);
        // Skip nodes which are marked as deleted.
        if ($wrapper->field_deleted->value() == TRUE) {
          continue;
        }

        // The first undeleted node is skipped.
        // Note that this is the 'most recent' node due to ordering in a query.
        if (!$undeleted_found) {
          $undeleted_found = TRUE;
          continue;
        }

        // Remaining undeleted nodes are marked as deleted.
        $wrapper->field_deleted->set(TRUE);
        $wrapper->save();
        $total_for_deletion++;
      }
      $deleted_for_encounter += $total_for_deletion;
    }

    if (round(memory_get_usage() / 1048576) >= $memory_limit) {
      drush_print(dt('Stopped before out of memory.'));
      return;
    }
  }
  drush_print("Total of $deleted_for_encounter duplicate measurements deleted for $encounter_type encounter.");
  $total_deleted += $deleted_for_encounter;
}

drush_print('------------------------------------------------------------');
drush_print("Done! Total of $total_deleted duplicate measurements deleted.");
