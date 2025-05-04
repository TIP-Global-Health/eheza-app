<?php

/**
 * @file
 * Generates adults and children CSV files.
 *
 * Generated format matches the one expected by 'import patients' utility.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/export-patients.php.
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
  filter_deleted($people);
  foreach ($people as $person) {
    $wrapper = entity_metadata_wrapper('node', $person);
    $person_id = $person->nid;

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
    list($first_name, $second_name) = resolve_name($wrapper);
    format_birth_date($birth_date);

    $estimated = $wrapper->field_birth_date_estimated->value();
    if (empty($estimated) || $estimated == FALSE) {
      $estimated = 'N';
    }
    else {
      $estimated = 'Y';
    }

    $gender = $wrapper->field_gender->value();
    $ubudehe = $wrapper->field_ubudehe->value();

    $hiv_status = $wrapper->field_hiv_status->value();
    switch ($hiv_status) {
      case 'hiv-exposed-infant':
        $hiv_status = 'hiv-exposed infant';
        break;

      case 'negative':
        $hiv_status = 'negative';
        break;

      case 'negative-dc':
        $hiv_status = 'negative - discordant couple';
        break;

      case 'positive':
        $hiv_status = 'positive';
        break;

      case 'unknown':
        $hiv_status = 'unknown';
    }

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
      switch ($mode_of_delivery) {
        case 'svd-episiotomy':
          $mode_of_delivery = 'spontaneous vaginal delivery with episiotomy';
          break;

        case 'svd-no-episiotomy':
          $mode_of_delivery = 'spontaneous vaginal delivery without episiotomy';
          break;

        case 'vd-vacuum':
          $mode_of_delivery = 'vaginal delivery with vacuum extraction';
          break;

        case 'cesarean-delivery':
          $mode_of_delivery = 'cesarean delivery';
      }

      $relation = 'parent';
      $relationships_ids = hedley_person_get_child_relationships($person_id, $relation);
      $relationships = node_load_multiple($relationships_ids);
      filter_deleted($relationships);
      $count = count($relationships);

      if ($count == 0) {
        $relation = 'caregiver';
        $relationships_ids = hedley_person_get_child_relationships($person_id, $relation);
        $relationships = node_load_multiple($relationships_ids);
        filter_deleted($relationships);
        $count = count($relationships);
      }

      if ($count == 0) {
        $mother_national_id = '';
        $mother_name = '';
        $relation = '';
      }
      else {
        if ($count > 1) {
          $instances = array_map(function ($node) {
            return $node->nid;
          }, $relationships);
          $instances_list = implode(', ', $instances);
          drush_print("Child with ID $person_id got $count relations of type $relation: $instances_list");
        }

        // Taking first relationship.
        $relationship = reset($relationships);
        $relationship_wrapper = entity_metadata_wrapper('node', $relationship);
        $adult_id = $relationship_wrapper->field_person->getIdentifier();
        $adult_wrapper = entity_metadata_wrapper('node', $adult_id);
        $mother_national_id = $adult_wrapper->field_national_id_number->value();
        list($first_name, $second_name) = resolve_name($adult_wrapper);
        $mother_name = trim("$first_name $second_name");
      }

      $mapping['children'][] = [
        $person_id,
        $first_name,
        $second_name,
        $birth_date,
        $estimated,
        $mother_national_id,
        $gender,
        $ubudehe,
        $hiv_status,
        $mode_of_delivery,
        $mother_name,
        $relation,
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
      $marital_status = $wrapper->field_marital_status->value();
      $number_of_children = $wrapper->field_number_of_children->value();
      $phone_number = $wrapper->field_phone_number->value();

      $education_level = $wrapper->field_education_level->value();
      switch ($education_level) {
        case '0':
          $education_level = 'no schooling';
          break;

        case '1':
          $education_level = 'primary school';
          break;

        case '2':
          $education_level = 'vocational training school';
          break;

        case '3':
          $education_level = 'secondary school';
          break;

        case '4':
          $education_level = 'diploma programs';
          break;

        case '5':
          $education_level = 'higher education';
          break;

        case '6':
          $education_level = 'advanced diploma';
      }

      $mapping['adults'][] = [
        $person_id,
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

  $count = count($people);
  $processed += $count;
  drush_print("$processed persons processed.");
}

// Write data into CSV files.
foreach ($mapping as $name => $rows) {
  $content = [];
  foreach ($rows as $row) {
    $content[] = implode(',', $row);
  }

  $path = drupal_get_path('module', 'hedley_migrate') . '/exported';
  $fp = fopen("$path/$name.csv", 'w');
  if ($name == 'adults') {
    $captions = 'Unique ID,First Name,Last Name,Date of Birth,Estimated (Y-N),National ID,Gender,Ubudehe,Education level,Marital Status,HIV Status,Number of Children,Phone,Province,District,Sector,Cell,Village,Health Center';
  }
  else {
    $captions = 'Unique ID,First Name,Last Name,Date of Birth,Estimated (Y-N),Mother National ID,Gender,Ubudehe,HIV Status,Mode of Delivery,Mother Name,Relation,Province,District,Sector,Cell,Village,Health Center,Group Name';
  }

  fwrite($fp, $captions . PHP_EOL);
  fwrite($fp, implode(PHP_EOL, $content));

  fclose($fp);
}

drush_print('Done!');

/**
 * Resolve first and second name of the person.
 *
 * Trims strings and removes all occurrences of ',', so that CSV
 * format is maintained.
 */
function resolve_name($wrapper) {
  $first_name = trim(str_replace(',', ' ', $wrapper->field_first_name->value()));
  $second_name = trim(str_replace(',', ' ', $wrapper->field_second_name->value()));
  if (empty($first_name) && empty($second_name)) {
    $second_name = trim(str_replace(',', ' ', $wrapper->label()));
  }

  return [$first_name, $second_name];
}

/**
 * Filters nodes that got 'deleted' field set to TRUE.
 */
function filter_deleted(&$nodes) {
  foreach ($nodes as $index => $node) {
    $wrapper = entity_metadata_wrapper('node', $node);
    $deleted = $wrapper->field_deleted->value();
    if (!empty($deleted) && $deleted == TRUE) {
      unset($nodes[$index]);
    }
  }
}

/**
 * Formats date as 'dd-MMM-YYY', where MMM is 'jan' - 'dec'.
 */
function format_birth_date(&$date) {
  $date = date('d-m-Y', $date);
  $parts = explode('-', $date);
  $mapping = [
    '01' => 'jan',
    '02' => 'feb',
    '03' => 'mar',
    '04' => 'apr',
    '05' => 'may',
    '06' => 'jun',
    '07' => 'jul',
    '08' => 'aug',
    '09' => 'sep',
    '10' => 'oct',
    '11' => 'nov',
    '12' => 'dec',
  ];

  $parts[1] = $mapping[$parts[1]];
  $date = implode('-', $parts);
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
