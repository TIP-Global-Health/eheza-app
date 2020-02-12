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

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

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

drush_print("Loading duplicate measurements for session $session. This may take several minutes...");

$base_query = new EntityFieldQuery();
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

        $last = array_pop($summary[$person][$date][$type]);
        drush_print("Person $person, on $date, $type: Total - $total, last NID - $last");
        $new_total = count($summary[$person][$date][$type]);
        node_delete_multiple($summary[$person][$date][$type]);
        drush_print("$new_total duplicates deleted");
        $count += $new_total;
      }
    }
  }
}

drush_print("Done! Total of $count duplicate measurements deleted.");
