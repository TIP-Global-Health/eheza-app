<?php

/**
 * @file
 * Generates sample DB (form currently installed DB).
 *
 * Execution: drush scr
 *  profiles/hedley/modules/custom/hedley_admin/scripts/generate-sample-db.php.
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
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes in DB.");
  exit;
}

drush_print("There are $count nodes at DB.");

// Get all image files from module directory
$module_path = drupal_get_path('module', 'hedley_migrate');
$images_path = $module_path . '/images';
$files = file_scan_directory($images_path, '/\.(jpg|jpeg|png|gif)$/i');
$files_array = array_values($files);
// Create a map to track managed files
$managed_files = [
  'boy' => [],
  'girl' => [],
  'father' => [],
  'mother' => [],
];
foreach ($files_array as $item) {
  $source = $item->uri;
  $file = (object) [
    'uid' => 1,
    'filename' => basename($source),
    'uri' => $source,
    'filemime' => file_get_mimetype($source),
    'status' => 1,
  ];

  // Copy file to public directory
  $directory = 'public://photos';
  file_prepare_directory($directory, FILE_CREATE_DIRECTORY);
  $file = file_copy($file, $directory);

  if ($file) {
    foreach (array_keys($managed_files) as $key) {
      if (strpos($source, $key) !== FALSE) {
        $managed_files[$key][] = $file;
        break;
      }
    }
  }
  else {
    drush_print("Failed to copy file: $source", 'error');
  }
}

$data = [
  'sample_health_centers_ids' => [4],
  'male_first_names' => hedley_migrate_male_first_names(),
  'female_first_names' => hedley_migrate_female_first_names(),
  'second_names' => hedley_migrate_second_names(),
  'bundles_to_delete' => [
    'child',
    'counseling_schedule',
    'counseling_topic',
    'mother',
    'participant_form',
    'report_data',
    'resilience_survey',
    'sync_incident',
    'whatsapp_record',
    'nurse',
    'device',
  ],
  'managed_files' => $managed_files,
];

$processed = 0;
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
    process_node($node, $data);
  }

  $nid = end($ids);
  $processed += count($nodes);

  // Explicitly unset large variables after use for memory optimization.
  unset($nodes);

  if ($processed % 5000 == 0) {
    drush_print("Processed $processed out of $count.");
  }

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done!");

function process_node($node, $data) {
  if ($node->type === 'catchment_area') {
    return;
  }

  $bundles_to_delete = $data['bundles_to_delete'];
  if (in_array($node->type, $bundles_to_delete)) {
    node_delete($node->nid);
    return;
  }

  $sample_health_centers_ids = $data['sample_health_centers_ids'];
  if ($node->type === 'health_center') {
    if (!in_array($node->nid, $sample_health_centers_ids)) {
      node_delete($node->nid);
    };
    return;
  }

  if ($node->type === 'village') {
    $health_center_id = $node->field_health_center[LANGUAGE_NONE][0]['target_id'];
    if (!in_array($health_center_id , $sample_health_centers_ids)) {
      node_delete($node->nid);
    };
    return;
  }

  if ($node->type === 'person') {
    $shards = sanitise_shards($node->field_shards[LANGUAGE_NONE], $sample_health_centers_ids);
    if (empty($shards)) {
      node_delete($node->nid);
    }
    else {
      $node->field_shards[LANGUAGE_NONE] = $shards;
      // Anonymise national ID.
      $node->field_national_id_number[LANGUAGE_NONE][0]['value'] = '';
      // Anonymise phone number.
      $node->field_phone_number[LANGUAGE_NONE][0]['value'] = '';
      // Anonymise name.
      $gender = $node->field_gender[LANGUAGE_NONE][0]['value'];
      if (!$gender) {
        $gender = 'female';
      }
      $first_name_key = $gender == 'male' ? array_rand($data['male_first_names']) : array_rand($data['female_first_names']);
      $first_name = $gender == 'male' ? $data['male_first_names'][$first_name_key] : $data['female_first_names'][$first_name_key];
      $second_name_key = array_rand($data['second_names']);
      $second_name = $data['second_names'][$second_name_key];
      $node->title = $second_name . ' ' . $first_name;
      $node->field_first_name[LANGUAGE_NONE][0]['value'] = $first_name;
      $node->field_second_name[LANGUAGE_NONE][0]['value'] = $second_name;
      // Replace photo.
      $current_file = field_get_items('node', $node, 'field_photo');
      if ($current_file) {
        $file = file_load($current_file[0]['fid']);
        if ($file) {
          file_delete($file);
        }
      }
      $birth_date = field_get_items('node', $node, 'field_birth_date');
      if ($birth_date) {
        $birth_timestamp = strtotime($birth_date[0]['value']);
        $age = (time() - $birth_timestamp) / (365 * 24 * 60 * 60);
        $is_adult = $age >= 13;
        if ($gender == 'male') {
          $folder = $is_adult ? 'father' : 'boy';
        }
        else {
          $folder = $is_adult ? 'mother' : 'girl';
        }
        $file_key = array_rand($data['managed_files'][$folder]);
        $file = $data['managed_files'][$folder][$file_key];
        $node->field_photo[LANGUAGE_NONE][0] = (array) $file;
      }

      try {
        node_save($node);
      }
      catch (\Exception $e) {
        drush_print("Error when saving person node ID $node->nid.");
      }
    }

    return;
  }

  // For any other node type.
  $shards = sanitise_shards($node->field_shards[LANGUAGE_NONE], $sample_health_centers_ids);
  if (empty($shards)) {
    node_delete($node->nid);
  }
  else {
    $node->field_shards[LANGUAGE_NONE] = $shards;
    if (strpos($node->type, 'photo')) {
      // Replace photo with the one from person profile.
      $current_file = field_get_items('node', $node, 'field_photo');
      if ($current_file) {
        $file = file_load($current_file[0]['fid']);
        if ($file) {
          file_delete($file);
        }
      }
      $person_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
      $person = node_load($person_id);
      if ($person) {
        $photo_field = field_get_items('node', $person, 'field_photo');
        if ($photo_field) {
          $node->field_photo[LANGUAGE_NONE][0] = $photo_field[0];
        }
      }
    }
    try {
      node_save($node);
    }
    catch (\Exception $e) {
      drush_print("Error when saving node ID $node->nid.");
    }
  }
}

function sanitise_shards($shards, $sample_health_centers_ids) {
  foreach ($shards as $index => $shard) {
    if (!in_array($shard['target_id'] , $sample_health_centers_ids)) {
      unset($shards[$index]);
    };
  }

  return array_values($shards);
}
