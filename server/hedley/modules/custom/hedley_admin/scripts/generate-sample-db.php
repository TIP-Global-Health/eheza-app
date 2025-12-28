<?php

/**
 * @file
 * Generates sample DB (from currently installed DB).
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
$batch = drush_get_option('batch', 2000);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 16000);


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

// Get all image files from module directory.
$module_path = drupal_get_path('module', 'hedley_migrate');
$images_path = $module_path . '/images';
$files = file_scan_directory($images_path, '/\.(jpg|jpeg|png|gif)$/i');
$files_array = array_values($files);
// Create a map to track managed files.
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

  // Copy file to private directory.
  $directory = 'private://photos';
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

$sample_health_centers_ids = [7091, 7092, 28589];

// Create a nurse for all sample health centers.
$hc_target_ids = [];
foreach ($sample_health_centers_ids as $index => $sample_health_centers_id) {
  $hc_target_ids[] = ['target_id' => $sample_health_centers_id];
}
$nurse = entity_create('node', [
  'type' => 'nurse',
  'title' => 'Sample Nurse',
  'field_pin_code' => [
    LANGUAGE_NONE => [
      [
        'value' => '12345',
      ],
    ],
  ],
  'field_role' => [
    LANGUAGE_NONE => [
      [
        'value' => 'nurse',
      ],
    ],
  ],
  'field_health_centers' => [
    LANGUAGE_NONE => $hc_target_ids,
  ],
]);
node_save($nurse);

// Create a CHW for all sample health centers villages.
$villages = [];
foreach ($sample_health_centers_ids as $sample_health_centers_id) {
  $villages = array_merge($villages, hedley_health_center_get_villages_by_health_center($sample_health_centers_id));
}
if (!empty($villages)) {
  $village_target_ids = [];
  foreach ($villages as $village) {
    $village_target_ids[] = ['target_id' => $village];
  }
  $chw = entity_create('node', [
    'type' => 'nurse',
    'title' => 'Sample CHW',
    'field_pin_code' => [
      LANGUAGE_NONE => [
        [
          'value' => '23456',
        ],
      ],
    ],
    'field_role' => [
      LANGUAGE_NONE => [
        [
          'value' => 'chw',
        ],
      ],
    ],
    'field_health_centers' => [
      LANGUAGE_NONE => $hc_target_ids,
    ],
    'field_villages' => [
      LANGUAGE_NONE => $village_target_ids,
    ],
  ]);
  node_save($chw);
}

$data = [
  'sample_health_centers_ids' => $sample_health_centers_ids,
  'sample_nurse' => $nurse->nid,
  'sample_chw' => !empty($chw) ? $chw->nid : NULL,
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
    'device',
  ],
  'managed_files' => $managed_files,
];

$processed = $kept = 0;
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
    $kept += process_node($node, $data);
  }

  $nid = end($ids);
  $processed += count($nodes);

  // Explicitly unset large variables after use for memory optimization.
  unset($nodes);

  $memory = round(memory_get_usage() / 1048576);

  if ($processed % 5000 == 0) {
    drush_print("Processed $processed out of $count. Nodes kept: $kept. Memory usage: $memory.");
  }

  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

// Create Gizra Admin.
$role = user_role_load_by_name('administrator');
$gizra_admin = [
  'name' => 'gizraAdmin',
  'mail' => 'gizraAdmin@example.com',
  'pass' => '2ASC$Hvm',
  'roles' => [$role->rid => $role->name],
  'status' => 1,
];
user_save(NULL, $gizra_admin);

drush_print("Done! Nodes kept: $kept.");

/**
 * Performs logic for provided node.
 */
function process_node($node, $data) {
  if ($node->type === 'catchment_area') {
    return 1;
  }

  $bundles_to_delete = $data['bundles_to_delete'];
  if (in_array($node->type, $bundles_to_delete)) {
    node_delete($node->nid);
    return 0;
  }

  if ($node->type === 'nurse') {
    if (!in_array($node->nid, [$data['sample_nurse'], $data['sample_chw']])) {
      node_delete($node->nid);
      return 0;
    }
    return 1;
  }

  $sample_health_centers_ids = $data['sample_health_centers_ids'];
  if ($node->type === 'health_center') {
    if (!in_array($node->nid, $sample_health_centers_ids)) {
      node_delete($node->nid);
      return 0;
    };
    return 1;
  }

  if ($node->type === 'village') {
    $health_center_id = $node->field_health_center[LANGUAGE_NONE][0]['target_id'];
    if (!in_array($health_center_id, $sample_health_centers_ids)) {
      node_delete($node->nid);
      return 0;
    };
    return 1;
  }

  if (!keep_by_shards($node->field_shards[LANGUAGE_NONE], $sample_health_centers_ids)) {
    node_delete($node->nid);
    return 0;
  }

  $save_required = FALSE;

  // For nodes that got nurse field, set it to sample nurse (since other
  // nurses are being deleted).
  if (!empty(field_info_instance('node', 'field_nurse', $node->type))) {
    $node->field_nurse[LANGUAGE_NONE][0]['target_id'] = $data['sample_nurse'];
    $save_required = TRUE;
  }

  if ($node->type === 'person') {
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
    $file = load_photo_for_person($node, $data);
    $node->field_photo[LANGUAGE_NONE][0] = $file;
    $save_required = TRUE;
  }
  elseif (strpos($node->type, 'photo') !== FALSE) {
    // Replace photo with the one from person profile.
    $current_file = field_get_items('node', $node, 'field_photo');
    if ($current_file) {
      $file = file_load($current_file[0]['fid']);
      if ($file) {
        file_delete($file);
        $save_required = TRUE;
      }
    }
    $person_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
    $person = node_load($person_id);
    if ($person) {
      // Replace image.
      $file = load_photo_for_person($person, $data);
      $node->field_photo[LANGUAGE_NONE][0] = $file;
      $save_required = TRUE;
    }
  }

  if (!$save_required) {
    return 1;
  }

  try {
    node_save($node);
    return 1;
  }
  catch (\Exception $e) {
    drush_print("Error when saving node ID $node->nid.");
    return 0;
  }
}

/**
 * Keep node, if any of it's shards appears in sample HCs list.
 */
function keep_by_shards($shards, $sample_health_centers_ids) {
  if (empty($shards)) {
    return FALSE;
  }

  foreach ($shards as $shard) {
    if (in_array($shard['target_id'], $sample_health_centers_ids)) {
      return TRUE;
    }
  }

  return FALSE;
}

/**
 * Load appropriate photo for person, per age and gender.
 */
function load_photo_for_person($node, $data) {
  $birth_date = field_get_items('node', $node, 'field_birth_date');
  if (empty($birth_date)) {
    $file_key = array_rand($data['managed_files']['mother']);
    return (array) $data['managed_files']['mother'][$file_key];
  }

  $gender = $node->field_gender[LANGUAGE_NONE][0]['value'];
  if (!$gender) {
    $gender = 'female';
  }
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
  return (array) $data['managed_files'][$folder][$file_key];
}
