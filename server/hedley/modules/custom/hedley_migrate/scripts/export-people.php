<?php

/**
 * @file
 *
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/export-people.php.
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

$base_query = base_query_for_bundle('person');
$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no patients in DB.");
  exit;
}

drush_print("$total patients located.");

$mapping = [
  'adults' => [],
  'children' => [],
];
$processed = 0;

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
  $people = node_load_multiple($ids);
  foreach ($people as $person) {
    $wrapper = entity_metadata_wrapper('node', $person);

    $deleted = $wrapper->field_deleted->value();
    if ($deleted) {
      // Skip deleted patient.
      continue;
    }

    $birth_date = $wrapper->field_birth_date->value();
    if (empty($birth_date)) {
      // Can't determine if adult or child. Skipping.
      continue;
    }

    $health_center_id = $wrapper->field_health_center->getIdentifier();
    if (empty($health_center_id)) {
      // Can't determine health center. Skipping.
      continue;
    }

    $is_child = $birth_date > strtotime('-13 year');

    // Common values.
    $first_name = trim($wrapper->field_first_name->value());
    $second_name = trim($wrapper->field_second_name->value());
    if (empty($first_name) && empty($second_name)) {
      $second_name = $wrapper->label();
    }
    $birth_date = date('Y-m-d', $birth_date);
    $estimated = $wrapper->field_birth_date_estimated->value();
    $gender = $wrapper->field_gender->value();
    $ubudehe = $wrapper->field_ubudehe->value();
    $hiv_status = $wrapper->field_hiv_status->value();
    $province = $wrapper->field_province->value();
    $district = $wrapper->field_district->value();
    $sector = $wrapper->field_sector->value();
    $cell = $wrapper->field_cell->value();
    $village = $wrapper->field_village->value();

    $health_center_id = $wrapper->field_health_center->getIdentifier();
    $health_center_wrapper = entity_metadata_wrapper('node', $health_center_id);
    $health_center = $health_center_wrapper->label();

    if ($is_child) {
      $mode_of_delivery = $wrapper->field_mode_of_delivery->value();

      $mapping['children'][] = [
        $person->nid,
        $first_name,
        $second_name,
        $birth_date,
        $estimated,
        //$mother_id,
        $gender,
        $ubudehe,
        $hiv_status,
        $mode_of_delivery,
        //$mother_id,
        //$relation,
        $province,
        $district,
        $sector,
        $cell,
        $village,
        $health_center,
      ];
    }
    else {
      $national_id = $wrapper->field_national_id_number->value();
      $education_level = $wrapper->field_education_level->value();
      $marital_status = $wrapper->field_marital_status->value();
      $number_of_children = $wrapper->field_number_of_children->value();
      $phone_number = $wrapper->field_phone_number->value();

      $mapping['adults'][] = [
        $person->nid,
        $first_name,
        $second_name,
        $birth_date,
        $estimated,
        $national_id,
        $gender,
        $ubudehe,
        $education_level,
        $marital_status,
        $hiv_status,
        $number_of_children,
        $phone_number,
        $province,
        $district,
        $sector,
        $cell,
        $village,
        $health_center,
      ];
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

drush_print('Done!');


foreach ($mapping as $name => $rows) {
  $content = [];
  foreach ($rows as $row) {
    $content[] = implode(',', $row);
  }

  $path = drupal_get_path('module', 'hedley_migrate') . '/exported';
  $fp = fopen("$path/$name.csv", 'w');
  fwrite($fp, implode(PHP_EOL, $content));

  fclose($fp);
}


/**
 * Generate base query.
 */
function base_query_for_bundle($bundle) {
  $query = new EntityFieldQuery();
  $query
    ->entityCondition('entity_type', 'node')
    ->propertyCondition('type', $bundle)
    ->propertyCondition('status', NODE_PUBLISHED);

  return $query;
}
