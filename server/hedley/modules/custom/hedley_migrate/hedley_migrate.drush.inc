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
    'callback' => 'hedley_create_sessions_today',
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
function hedley_create_sessions_today() {
  // Get all the clinic IDs.
  $clinicQuery = new EntityFieldQuery();
  $clinicResult = $clinicQuery
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'clinic')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->range(0, 200)
    ->execute();

  if (!$clinicResult['node']) {
    // No clinics, so just bail.
    return;
  }

  // Pick out the IDs.
  $clinicIds = array_keys($clinicResult['node']);

  $today = date("Y-m-d");

  // Get all the existing sessions that start today already. We only
  // store the date, rather than a time, so the "=" should work.
  $sessionQuery = new EntityFieldQuery();
  $sessionResult = $sessionQuery
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'session')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_scheduled_date', 'value', $today, '=')
    ->range(0, 200)
    ->execute();

  // Now, remove all the clinicIds that already have a session today.
  // There is probably a more efficient way to do this.
  if ($sessionResult['node']) {
    foreach (node_load_multiple(array_keys($sessionResult['node'])) as $session) {
      $wrapper = entity_metadata_wrapper('node', $session);
      $clinicIds = array_diff($clinicIds, [$wrapper->field_clinic->getIdentifier()]);
    }
  }

  // Finally, actually create the new Session for each clinic that didn't
  // already have one for today.
  foreach ($clinicIds as $clinicId) {
    $node = entity_create('node', [
      'type' => "session",
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_clinic->set($clinicId);

    // For now, just permit data entry today.
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
