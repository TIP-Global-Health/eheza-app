<?php

/**
 * @file
 * Recalculate all shards.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-impacted-patients.php.
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

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'person')
  ->propertyCondition('status', NODE_PUBLISHED);

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no people in DB.");
  exit;
}

drush_print("$total people located.");

$impacted_patients = [
  'lt1m' => 0,
  'lt2y' => 0,
  'lt5y' => 0,
  'lt10y' => 0,
  'lt20y' => 0,
  'lt50y' => 0,
  'mt50y' => 0,
];
$processed = 0;
$measurement_types = hedley_admin_get_measurement_types();

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
  foreach ($ids as $id) {
    $measurements_ids = hedley_admin_get_person_measurements($id, $measurement_types);
    if (empty($measurements_ids)) {
      continue;
    }

    if (count($measurements_ids) > 50) {
      $classification = classify_by_age($id);
      $impacted_patients[$classification]++;
      continue;
    }

    $measurements = node_load_multiple($measurements_ids);
    $first_timestamp = array_shift($measurements)->created;

    foreach($measurements as $measurement) {
      if (abs($first_timestamp - $measurement->created) > 7*24*3600) {
        $classification = classify_by_age($id);
        $impacted_patients[$classification]++;
        break;
      }
    }
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  drush_print("$processed persons processed.");
}

drush_print('Done! Impacted patients:');
$count = $impacted_patients['lt1m'];
drush_print("* 0 - 1 month: $count");
$count = $impacted_patients['lt2y'];
drush_print("* 1 month - 2 years: $count");
$count = $impacted_patients['lt5y'];
drush_print("* 2 years - 5 years: $count");
$count = $impacted_patients['lt10y'];
drush_print("* 5 years - 10 years: $count");
$count = $impacted_patients['lt20y'];
drush_print("* 10 years - 20 years: $count");
$count = $impacted_patients['lt50y'];
drush_print("* 20 years - 50 years: $count");
$count = $impacted_patients['mt50y'];
drush_print("* Older than 50 years: $count");

function classify_by_age($person_id) {
  $wrapper = entity_metadata_wrapper('node', $person_id);
  $birth_date = $wrapper->field_birth_date->value();

  if ($birth_date > strtotime('-1 month')) {
    return 'lt1m';
  }
  else if ($birth_date > strtotime('-2 year')) {
    return 'lt2y';
  }
  else if ($birth_date > strtotime('-5 year')) {
    return 'lt5y';
  }
  else if ($birth_date > strtotime('-10 year')) {
    return 'lt10y';
  }
  else if ($birth_date > strtotime('-20 year')) {
    return 'lt20y';
  }
  else if ($birth_date > strtotime('-50 year')) {
    return 'lt50y';
  }

  return 'mt50y';
}
