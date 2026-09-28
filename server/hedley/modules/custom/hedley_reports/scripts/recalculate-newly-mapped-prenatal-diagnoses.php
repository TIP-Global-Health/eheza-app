<?php

/**
 * @file
 * Queues report recalculation for people whose diagnoses gained a code.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-newly-mapped-prenatal-diagnoses.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// These gained a code. An unmapped diagnosis is dropped when report data is
// built, so their encounters count wrongly until it is built again.
$diagnoses = [
  'hyperemesis-gravidum-by-symptoms',
  'severe-vomiting-by-symptoms',
  'high-risk-of-preeclampsia-initial',
  'high-risk-of-preeclampsia-recurrent',
  'moderate-risk-of-preeclampsia',
];

// Report without queueing anything.
$dry_run = drush_get_option('dry_run', FALSE);

// Encounter, participant and person are each skipped when deleted. Each needs
// its own join alias, so the deleted field is joined here.
$query = db_select('node', 'n');
$query->join('field_data_field_prenatal_diagnoses', 'd', 'd.entity_id = n.nid');
$query->join('field_data_field_individual_participant', 'ip', 'ip.entity_id = n.nid');
$query->join('node', 'pt', 'pt.nid = ip.field_individual_participant_target_id');
$query->join('field_data_field_person', 'p', 'p.entity_id = pt.nid');
$query->join('node', 'ps', 'ps.nid = p.field_person_target_id');

foreach (['n' => 'fdn', 'pt' => 'fdt', 'ps' => 'fds'] as $node_alias => $deleted_alias) {
  $query->leftJoin('field_data_field_deleted', $deleted_alias, "$node_alias.nid = $deleted_alias.entity_id");
  $query->condition(db_or()
    ->isNull("$deleted_alias.field_deleted_value")
    ->condition("$deleted_alias.field_deleted_value", 0)
  );
}

$query->condition('ps.status', NODE_PUBLISHED);
$query->condition('d.field_prenatal_diagnoses_value', $diagnoses, 'IN');
$query->addField('p', 'field_person_target_id', 'person_id');
$query->distinct();

$person_ids = $query->execute()->fetchCol();

if (empty($person_ids)) {
  drush_print('No person carries one of these diagnoses.');
  return;
}

$count = count($person_ids);
drush_print("$count people carry one of these diagnoses.");

if ($dry_run) {
  drush_print('Dry run: nothing was queued.');
  return;
}

$queued = 0;
foreach ($person_ids as $person_id) {
  $added = hedley_general_add_task_to_advanced_queue_by_id(HEDLEY_REPORTS_CALCULATE_AGGREGATED_DATA, $person_id, [
    'person_id' => $person_id,
  ]);

  if ($added) {
    $queued++;
  }
}

$skipped = $count - $queued;
drush_print("Done! Queued $queued, already queued $skipped.");
