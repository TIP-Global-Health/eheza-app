<?php

/**
 * @file
 * Generate Hedley demo content via Drush.
 */

/**
 * Hook for defining drush commands.
 */
function hedley_migrate_drush_command() {
  $items['delete-child-measurements'] = [
    'callback' => 'hedley_delete_child_measurements',
    'description' => 'Delete all measurements for the given child ID.',
    'arguments' => [
      'child_id' => 'The ID of the child whose measurements will be deleted.',
    ],
    'options' => [],
    'aliases' => ['dcm'],
  ];

  // This is a drush command so you can easily create sample sessions for today.
  $items['create-sessions-today'] = [
    'callback' => 'hedley_migrate_create_sessions_today',
    'description' => 'Create a session for today for all clinics (for devel)',
    'arguments' => [],
    'options' => [],
    'aliases' => ['cst'],
  ];

  return $items;
}

/**
 * Creates a session for today for every clinic, if not already existing.
 *
 * This is for devel purposes (and possibly useful for testing).
 */
function hedley_migrate_create_sessions_today() {
  // Get all the clinic IDs.
  $clinic_query = new EntityFieldQuery();
  $clinic_result = $clinic_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'clinic')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->range(0, 200)
    ->execute();

  if (!$clinic_result['node']) {
    // No clinics, so just bail.
    return;
  }

  // Pick out the IDs.
  $clinic_ids = array_keys($clinic_result['node']);

  $session_ids = hedley_schedule_get_today_sessions(200);

  // Now, remove all the clinicIds that already have a session today.
  // There is probably a more efficient way to do this.
  foreach (node_load_multiple($session_ids) as $session_id) {
    $wrapper = entity_metadata_wrapper('node', $session_id);
    $clinic_ids = array_diff($clinic_ids, [$wrapper->field_clinic->getIdentifier()]);
  }

  // Finally, actually create the new Session for each clinic that didn't
  // already have one for today.
  foreach ($clinic_ids as $clinic_id) {
    $node = entity_create('node', [
      'type' => "session",
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_clinic->set($clinic_id);

    // For now, just permit data entry today.
    $today = date("Y-m-d");
    $wrapper->field_scheduled_date->set([
      'value' => $today,
      'value2' => $today,
    ]);

    $wrapper->save();
  }

}

/**
 * Deletes all child measurements for the given child ID.
 */
function hedley_delete_child_measurements($child_id) {
  $bundles = [
    'height',
    'weight',
    'muac',
    'nutrition',
    'photo',
  ];

  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_child', 'target_id', $child_id)
    ->range(0, 200)
    ->execute();

  if ($result['node']) {
    node_delete_multiple(array_keys($result['node']));
  }
}
