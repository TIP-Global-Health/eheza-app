<?php

/**
 * @file
 * Locates duplicate measurements from session.
 *
 * Execution:
 *   drush scr profiles/hedley/modules/custom/hedley_admin/scripts/
 *             locate-duplicate-measurements.php --session=[session ID].
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

$types = [
  'height' => 'field_height' ,
  'weight' => 'field_weight',
  'photo' => 'field_photo',
  'attendance' => 'family_attended',
  'family_planning' => 'family_planning',
  'muac' => 'family_muac',
  'nutrition' => 'family_nutrition',
];

$session = drush_get_option('session', FALSE);
if (!$session) {
  drush_print('Please specify --session option');
  exit;
}

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', array_keys($types), 'IN')
  ->propertyOrderBy('nid', 'ASC')
  ->fieldCondition('field_session', 'target_id', $session);

if ($nid) {
  $base_query->propertyCondition('nid', $nid, '>');
}

$summary = [];
$people = [];
$dates = [];

while (TRUE) {
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

    $session = $wrapper->field_session->getIdentifier();
    $person = $wrapper->field_person->getIdentifier();
    $people[] = $person;
    $date = date('Y-m-d', $wrapper->field_date_measured->value());
    $dates[] = $date;
    $type = $node->type;

    if (empty($summary[$person])) {
      $summary[$person] = [];
    }

    if (empty($summary[$person][$date])) {
      $summary[$person][$date] = [];
    }

    if (empty($summary[$person][$date][$type])) {
      $summary[$person][$date][$type] = [];
    }

    $summary[$person][$date][$type][] = $node->nid;
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

$count = 0;
foreach (array_unique($people) as $person) {
  foreach (array_unique($dates) as $date) {
    foreach (array_keys($types) as $type) {
      if (!empty($summary[$person][$date][$type])) {
        $total = count($summary[$person][$date][$type]);
        if ($total < 2) {
          continue;
        }

        $new_total = count($summary[$person][$date][$type]);
        $count += $new_total;
      }
    }
  }
}

if ($count > 0) {
  drush_print("Session $session got $count duplicate measurements.");
}
else {
  drush_print("No duplicate measurements at session $session.");
}
