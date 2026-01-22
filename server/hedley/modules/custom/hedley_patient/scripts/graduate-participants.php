<?php

/**
 * @file
 * Adds graduation date for group participants.
 *
 * - Child is considered a graduate when over 26 month old.
 * - Sorwathe participants do not graduate.
 *
 * Execution:
 *   drush scr profiles/hedley/modules/custom/hedley_patient/scripts/
 *             graduate-participants.php
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$type = 'pmtct_participant';

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

drush_print("$total nodes of type $type located.");

$processed = 0;
$graduated = 0;
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '>');
  }

  $result = $query
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    // No more items left.
    break;
  }

  $ids = array_keys($result['node']);
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $wrapper = entity_metadata_wrapper('node', $node);

    $expected = $wrapper->field_expected->value();
    // We do not change graduation date if it's already set.
    if (!empty($expected['value2'])) {
      continue;
    }

    $clinic_type = $wrapper->field_clinic->field_group_type->value();
    // We do not set graduation date for sorwathe/achi group participants.
    if (!hedley_person_graduation_date_required($clinic_type)) {
      continue;
    }

    $child_birth_date = $wrapper->field_person->field_birth_date->value();
    // We can't set graduation date, unless we know childs birth date.
    if (empty($child_birth_date)) {
      continue;
    }

    $wrapper->field_expected->set([
      'value' => $expected['value'],
      'value2' => date('Y-m-d', strtotime("+26 month", $child_birth_date)),
    ]);
    $wrapper->save();
    $graduated++;
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($nodes);
  $processed += $count;
  drush_print("$count nodes of type $type processed.");
}

drush_print("Done! $graduated participants graduated.");
