<?php

/**
 * @file
 * Writes the HealthyStart dashboard record of antenatal pregnancies.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/dashboard-generate-pregnancy-data.php.
 *
 * Options: --nid, --batch, --exclude_set=1 (skip pregnancies with a record),
 * --open=1 (unconcluded only), --memory_limit.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

if (!hedley_reports_dashboard_data_enabled()) {
  drush_print('Healthy Start is not enabled on this site; nothing to generate.');
  return;
}

$nid = drush_get_option('nid', 0);
$batch = drush_get_option('batch', 50);
$exclude_set = drush_get_option('exclude_set', FALSE);
$open_only = drush_get_option('open', FALSE);
$memory_limit = drush_get_option('memory_limit', 800);

$base_query = hedley_general_create_db_select_query_excluding_deleted();
$base_query->join('field_data_field_encounter_type', 'et', 'et.entity_id = n.nid');
$base_query->condition('n.type', HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE);
$base_query->condition('n.status', NODE_PUBLISHED);
$base_query->condition('et.field_encounter_type_value', HEDLEY_STATS_PRENATAL_ENCOUNTER_TYPE);

$first_cohort = hedley_reports_dashboard_first_cohort_timestamp();
if ($first_cohort) {
  $base_query->condition('n.created', $first_cohort, '>=');
}

if ($exclude_set) {
  $base_query->leftJoin('field_data_field_reports_data', 'rd', 'rd.entity_id = n.nid');
  $base_query->isNull('rd.field_reports_data_value');
}

if ($open_only) {
  $base_query->leftJoin('field_data_field_date_concluded', 'dc', 'dc.entity_id = n.nid');
  $base_query->isNull('dc.field_date_concluded_value');
}

$count_query = clone $base_query;
$count_query->condition('n.nid', $nid, '>');
$count_query->addExpression('COUNT(*)', 'cnt');
$count = (int) $count_query->execute()->fetchField();

if ($count == 0) {
  drush_print('There are no pregnancies to generate dashboard data for.');
  return;
}

$total = 0;
drush_print("$count pregnancies located.");

while (TRUE) {
  $query = clone $base_query;
  $query->condition('n.nid', $nid, '>');
  $query->addField('n', 'nid');
  $query->orderBy('n.nid');
  $query->range(0, $batch);
  $ids = $query->execute()->fetchCol();

  if (empty($ids)) {
    // No more items left.
    break;
  }

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    if (hedley_reports_generate_dashboard_data_for_pregnancy($node, $exclude_set)) {
      $total++;
    }

    $memory = round(memory_get_usage() / 1048576);
    if ($memory >= $memory_limit) {
      drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
      return;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  drush_print("Calculated so far: $total, Memory: $memory");

  // Free up memory.
  drupal_static_reset();

  $nid = end($ids);
}

drush_print("Done! Dashboard data calculated for $total pregnancies.");
