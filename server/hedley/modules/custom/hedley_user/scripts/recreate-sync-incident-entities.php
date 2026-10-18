<?php

/**
 * @file
 * Re-creates missing entities named in incident details stored on users.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_user/scripts/
 *   recreate-sync-incident-entities.php [--dry_run].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what is missing, without saving anyone.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Saving a user re-creates the missing entities its details name.
$uids = db_select('field_data_field_incident_details', 'd')
  ->fields('d', ['entity_id'])
  ->condition('d.entity_type', 'user')
  ->condition('d.deleted', 0)
  ->execute()
  ->fetchCol();

if (empty($uids)) {
  drush_print('No user holds incident details.');
  return;
}

$bundles = hedley_user_sync_incident_bundles();

/**
 * The UUIDs in a user's incident details that the backend lacks.
 *
 * Only content types that re-creation handles are listed, each UUID once.
 */
$missing_uuids = function ($account) use ($bundles) {
  if (empty($account->field_incident_details[LANGUAGE_NONE][0]['value'])) {
    return [];
  }

  $entities = json_decode($account->field_incident_details[LANGUAGE_NONE][0]['value']);
  if (!is_array($entities)) {
    return [];
  }

  $missing = [];
  foreach ($entities as $data) {
    if (empty($data->uuid) || empty($data->type) || !in_array($data->type, $bundles)) {
      continue;
    }

    if (hedley_restful_resolve_nid_for_uuid($data->uuid) === FALSE) {
      $missing[$data->uuid] = $data->uuid;
    }
  }

  return array_values($missing);
};

// Each missing UUID once, though several users may hold it.
$missing_before = [];
$users = 0;
foreach ($uids as $uid) {
  $account = user_load($uid, TRUE);
  $uuids = $missing_uuids($account);
  if (empty($uuids)) {
    continue;
  }

  $before = count($uuids);
  $users++;
  $missing_before += array_fill_keys($uuids, TRUE);

  if ($dry_run) {
    drush_print("User $uid: $before missing.");
    continue;
  }

  // The presave hook reads the details from $edit only, so they are passed
  // in. Saving the account as it is would re-create nothing.
  try {
    user_save($account, [
      'field_incident_details' => $account->field_incident_details,
    ]);
  }
  catch (Exception $e) {
    drush_print("User $uid: the save failed, nothing re-created: " . $e->getMessage());
    continue;
  }

  $uuids = array_values(array_filter($uuids, function ($uuid) {
    return hedley_restful_resolve_nid_for_uuid($uuid) === FALSE;
  }));
  $after = count($uuids);
  $recreated = $before - $after;
  drush_print("User $uid: re-created $recreated of $before, still missing $after.");
}

if ($users == 0) {
  drush_print('Incident details name no missing entity.');
  return;
}

$before_total = count($missing_before);
if ($dry_run) {
  drush_print("Dry run: $users users hold $before_total missing entities. Nobody was saved.");
  return;
}

$after_total = 0;
foreach (array_keys($missing_before) as $uuid) {
  if (hedley_restful_resolve_nid_for_uuid($uuid) === FALSE) {
    $after_total++;
  }
}

$recreated_total = $before_total - $after_total;
drush_print("Done! Re-created $recreated_total of $before_total, on $users users.");
if ($after_total > 0) {
  drush_print("$after_total could not be re-created and stay in the details; see watchdog type hedley_user for the reason.");
}
