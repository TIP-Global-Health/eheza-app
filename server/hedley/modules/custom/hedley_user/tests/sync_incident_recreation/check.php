<?php

/**
 * @file
 * Checks that every content type a device can report missing is re-created.
 *
 * For each bundle that hedley_user_user_presave() re-creates, a node with
 * every field filled is rendered as /api/sync renders it, and run.js turns
 * that into the record a device holds. The node is hard-deleted, and
 * re-created from the record through user_save(), as the incident details
 * endpoint does. The re-created node goes through the same steps, and the
 * record a device would then hold must equal the one it sent.
 *
 * Runs twice, with every boolean FALSE and then TRUE, each in a transaction
 * that is rolled back. Fails the drush command on any unexpected result.
 *
 * Usage: drush php-script check.php, with run.js and worker.js beside it.
 */

// Record keys a re-created entity is known to lose.
$known_keys = [
  'photo' => 'A device sends the URL of a photo, not the file, and image fields are not re-created.',
];

// Content types known to be lost, which cannot be reported missing in
// practice. They are not checked, so their failure does not fill the log.
$not_checked = [
  'acute_illness_contacts_tracing' => 'No content of this type exists. Its multi-value text field is written as one value, so the insert fails.',
  'counseling_session' => 'Nothing references a counseling session, so it is never reported missing. Only the first of its topics is kept.',
];

$failures = [];
foreach ([FALSE, TRUE] as $bool) {
  $label = $bool ? 'TRUE' : 'FALSE';
  $results = sync_incident_check_run($bool, $known_keys, array_keys($not_checked));
  $passed = 0;
  foreach ($results as $bundle => $problems) {
    if (empty($problems)) {
      $passed++;
    }
    else {
      $failures["$bundle (booleans $label)"] = $problems;
    }
  }
  drush_print(format_string('Booleans @label: @passed of @total content types re-created as sent.', [
    '@label' => $label,
    '@passed' => $passed,
    '@total' => count($results),
  ]));
}

foreach ($known_keys as $key => $reason) {
  drush_print("Not compared: $key. $reason");
}
foreach ($not_checked as $bundle => $reason) {
  drush_print("Not checked: $bundle. $reason");
}

foreach ($failures as $name => $problems) {
  drush_print("FAILED: $name");
  foreach ($problems as $problem) {
    drush_print('  ' . $problem);
  }
}

if ($failures) {
  drush_set_error('SYNC_INCIDENT_RECREATION', format_string('@count re-creations, over both boolean runs, do not match what the device sent.', ['@count' => count($failures)]));
}

/**
 * Re-creates every bundle from its device record.
 *
 * @param bool $bool
 *   The value of every boolean field.
 * @param array $known_keys
 *   Record keys not compared, as their loss is known.
 * @param array $not_checked
 *   Bundles left out.
 *
 * @return array
 *   Problems found, keyed by bundle. An empty list means the bundle passed.
 */
function sync_incident_check_run($bool, array $known_keys, array $not_checked) {
  $dir = __DIR__;
  $account = user_load(1);
  $GLOBALS['user'] = $account;
  // Lets node_delete() run. Set for this request only.
  $GLOBALS['conf']['hedley_super_user_mode'] = TRUE;

  $bundles = array_diff(array_merge(
    [
      'person',
      'individual_participant',
      'pmtct_participant',
      'family_participant',
    ],
    hedley_general_get_encounter_types(),
    hedley_general_get_measurement_types()
  ), $not_checked);
  $handlers = HEDLEY_RESTFUL_ALL_DEVICES + HEDLEY_RESTFUL_SHARDED;
  $problems = array_fill_keys($bundles, []);

  $file = file_save_data(base64_decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=='), 'public://sync-incident-check.png', FILE_EXISTS_RENAME);
  $transaction = db_transaction();
  try {
    // Save and render a node of every bundle.
    $sources = [];
    $rendered = [];
    foreach ($bundles as $bundle) {
      if (empty($handlers[$bundle])) {
        $problems[$bundle][] = 'No sync handler.';
        continue;
      }
      $uuid = sync_incident_check_uuid();
      $node = entity_create('node', [
        'type' => $bundle,
        'uid' => 1,
        'status' => NODE_PUBLISHED,
      ]);
      foreach (field_info_instances('node', $bundle) as $instance) {
        $name = $instance['field_name'];
        if ($name == 'field_shards') {
          continue;
        }
        $info = field_info_field($name);
        $items = $name == 'field_uuid' ? [['value' => $uuid]] : sync_incident_check_items($name, $info, $bool, $file->fid);
        if ($items === NULL) {
          if ($instance['required']) {
            $problems[$bundle][] = "No value for required $name.";
          }
          continue;
        }
        $node->{$name}[LANGUAGE_NONE] = $items;
      }
      try {
        node_save($node);
      }
      catch (Exception $e) {
        $problems[$bundle][] = 'Saving the source node failed: ' . $e->getMessage();
        continue;
      }
      $rendered[] = sync_incident_check_render($handlers[$bundle], $node->nid, $account);
      $sources[$bundle] = ['nid' => $node->nid, 'uuid' => $uuid];
    }
    $sent = sync_incident_check_device_records($dir, $rendered);

    // Delete each node, and re-create it from what the device sends.
    $recreated = [];
    foreach ($sources as $bundle => $source) {
      $uuid = $source['uuid'];
      if (empty($sent[$uuid]['details'])) {
        $problems[$bundle][] = 'The device cannot decode it: ' . substr($sent[$uuid]['decodeError'] ?? 'no output', 0, 300);
        continue;
      }

      node_delete($source['nid']);
      entity_get_controller('node')->resetCache();
      drupal_static_reset('hedley_restful_resolve_nid_for_uuid');

      $wid = (int) db_query('SELECT MAX(wid) FROM {watchdog}')->fetchField();
      $notices = [];
      set_error_handler(function ($number, $message, $file, $line) use (&$notices) {
        if (strpos($file, '/hedley/modules/custom/') !== FALSE) {
          $notices[] = "$message at " . basename($file) . ":$line";
        }
        return TRUE;
      });
      $saved = user_save(user_load(1, TRUE), [
        'field_incident_details' => [LANGUAGE_NONE => [['value' => $sent[$uuid]['details']]]],
      ]);
      restore_error_handler();
      drupal_static_reset('hedley_restful_resolve_nid_for_uuid');

      foreach (array_unique($notices) as $notice) {
        $problems[$bundle][] = "PHP notice: $notice";
      }
      $nid = hedley_restful_resolve_nid_for_uuid($uuid);
      if (!$nid) {
        $problems[$bundle][] = 'Not re-created.';
        $logs = db_query('SELECT type, message, variables FROM {watchdog} WHERE wid > :wid AND severity <= :severity', [
          ':wid' => $wid,
          ':severity' => WATCHDOG_WARNING,
        ]);
        foreach ($logs as $log) {
          $problems[$bundle][] = $log->type . ': ' . substr(strip_tags(format_string($log->message, unserialize($log->variables) ?: [])), 0, 300);
        }
        continue;
      }
      if (!empty($saved->field_incident_details[LANGUAGE_NONE])) {
        $problems[$bundle][] = 'The incident details were not cleared.';
      }
      $recreated[$bundle] = sync_incident_check_render($handlers[$bundle], $nid, $account);
    }
    $received = sync_incident_check_device_records($dir, array_values($recreated));

    // A device must hold the same record after the round trip.
    foreach (array_keys($recreated) as $bundle) {
      $uuid = $sources[$bundle]['uuid'];
      if (empty($received[$uuid]['details'])) {
        $problems[$bundle][] = 'The device cannot decode the re-created entity: ' . substr($received[$uuid]['decodeError'] ?? 'no output', 0, 300);
        continue;
      }
      $before = sync_incident_check_comparable($sent[$uuid]['details']);
      $after = sync_incident_check_comparable($received[$uuid]['details']);
      foreach (array_unique(array_merge(array_keys($before), array_keys($after))) as $key) {
        if (isset($known_keys[$key])) {
          continue;
        }
        $value_before = $before[$key] ?? NULL;
        $value_after = $after[$key] ?? NULL;
        if ($value_before !== $value_after) {
          $problems[$bundle][] = "$key: sent " . json_encode($value_before) . ', re-created ' . json_encode($value_after);
        }
      }
    }
  }
  finally {
    $transaction->rollback();
    file_delete($file, TRUE);
  }

  return $problems;
}

/**
 * Returns the field items a source node gets, or NULL to leave it empty.
 *
 * @param string $name
 *   The field name.
 * @param array $info
 *   The field info.
 * @param bool $bool
 *   The value of every boolean field.
 * @param int $fid
 *   The file ID for image fields.
 *
 * @return array|null
 *   The field items.
 */
function sync_incident_check_items($name, array $info, $bool, $fid) {
  // A re-created entity is never deleted.
  if ($name == 'field_deleted') {
    return [['value' => 0]];
  }
  $multi = $info['cardinality'] != 1;
  $count = $multi ? 2 : 1;

  switch ($info['type']) {
    case 'entityreference':
      $bundles = $info['settings']['handler_settings']['target_bundles'] ?? [];
      $nids = $bundles ? db_query_range('SELECT nid FROM {node} WHERE type IN (:bundles) AND status = 1 ORDER BY nid', 0, $count, [':bundles' => array_values($bundles)])->fetchCol() : [];
      return $nids ? array_map(function ($nid) {
        return ['target_id' => $nid];
      }, $nids) : NULL;

    case 'list_boolean':
      return [['value' => (int) $bool]];

    case 'list_text':
    case 'list_integer':
    case 'list_float':
      // "none" marks an empty set; a real choice checks more.
      $allowed = array_keys(list_allowed_values($info));
      $choices = array_values(array_diff($allowed, ['none'])) ?: $allowed;
      return $choices ? array_map(function ($value) {
        return ['value' => $value];
      }, array_slice($choices, 0, $count)) : NULL;

    case 'datetime':
      $item = ['value' => '2026-01-15 00:00:00'];
      if (!empty($info['settings']['todate'])) {
        $item['value2'] = '2026-02-20 00:00:00';
      }
      return $multi ? [$item, ['value' => '2026-03-10 00:00:00']] : [$item];

    case 'number_integer':
      return [['value' => 3]];

    case 'number_float':
      return [['value' => 2.5]];

    case 'text':
      $max = !empty($info['settings']['max_length']) ? $info['settings']['max_length'] : 255;
      $values = $multi ? ['Text a', 'Text b'] : ['Text'];
      return array_map(function ($value) use ($max) {
        return ['value' => substr($value, 0, $max)];
      }, $values);

    case 'text_long':
      return [['value' => 'Long text', 'format' => NULL]];

    case 'datestamp':
      return [['value' => 1768435200]];

    case 'image':
      return [['fid' => $fid]];
  }

  return NULL;
}

/**
 * Renders a node the way /api/sync does.
 *
 * @param string $handler_name
 *   The RESTful handler of the bundle.
 * @param int $nid
 *   The node ID.
 * @param object $account
 *   The user to render as.
 *
 * @return object
 *   The rendered item.
 */
function sync_incident_check_render($handler_name, $nid, $account) {
  $handler = restful_get_restful_handler($handler_name);
  $handler->setAccount($account);
  $items = $handler->viewWithDbSelect([$nid]);
  return reset($items);
}

/**
 * Returns the device record of each rendered item, keyed by UUID.
 *
 * @param string $dir
 *   The directory of run.js.
 * @param array $items
 *   Rendered items.
 *
 * @return array
 *   Each entry holds 'details', the incident details a device sends, or
 *   'decodeError'.
 *
 * @throws \Exception
 */
function sync_incident_check_device_records($dir, array $items) {
  $input = "$dir/input.json";
  // Kept apart from the output, as the Elm runtime warns there that it is
  // compiled in development mode.
  $errors = "$dir/errors.txt";
  file_put_contents($input, json_encode($items));
  $output = json_decode(shell_exec('node ' . escapeshellarg("$dir/run.js") . ' < ' . escapeshellarg($input) . ' 2> ' . escapeshellarg($errors)), TRUE);
  if (!is_array($output) || isset($output['fatal'])) {
    throw new Exception('run.js failed: ' . json_encode($output) . ' ' . file_get_contents($errors));
  }
  return $output;
}

/**
 * Returns a device record without the keys a new revision changes.
 *
 * @param string $details
 *   Incident details holding one record.
 *
 * @return array
 *   The record.
 */
function sync_incident_check_comparable($details) {
  $record = json_decode($details, TRUE)[0];
  unset($record['vid']);
  return $record;
}

/**
 * Returns a random version 4 UUID.
 *
 * @return string
 *   The UUID.
 */
function sync_incident_check_uuid() {
  $bytes = random_bytes(16);
  $bytes[6] = chr(ord($bytes[6]) & 0x0f | 0x40);
  $bytes[8] = chr(ord($bytes[8]) & 0x3f | 0x80);
  return vsprintf('%s%s-%s-%s-%s-%s%s%s', str_split(bin2hex($bytes), 4));
}
