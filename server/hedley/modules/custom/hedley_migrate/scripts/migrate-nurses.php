<?php

/**
 * @file
 * Migrate users to `nurse` content type..
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/migrate-nurses.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$nurse_role = user_role_load_by_name('nurse');
$admin_role = user_role_load_by_name('administrator');

$query = new EntityFieldQuery();
$query->entityCondition('entity_type', 'user');

$result = $query->execute();
$users = user_load_multiple(array_keys($result['user']));

foreach ($users as $account) {
  $roles = [];

  if (user_has_role($nurse_role->rid, $account)) {
    $roles[] = 'nurse';
  }

  if (user_has_role($admin_role->rid, $account)) {
    $roles[] = 'admin';
  }

  if (empty($roles)) {
    // Not an admin or nurse, so skip.
    continue;
  }

  $user_wrapper = entity_metadata_wrapper('user', $account);

  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'nurse')
    ->propertyCondition('title', $account->name)
    ->execute();

  if (empty($result['node'])) {
    $node = entity_create('node', ['type' => 'nurse']);
    $node_wrapper = entity_metadata_wrapper('node', $node);
  }
  else {
    $node_wrapper = entity_metadata_wrapper('node', key($result['node']));
  }

  $node_wrapper->title->set($account->name);
  $node_wrapper->field_email->set($account->mail);
  $node_wrapper->field_role->set($roles);
  $node_wrapper->field_clinics->set($user_wrapper->field_clinics->value());

  $node_wrapper->save();

  $params = [
    '@name' => $account->name,
    '@nid' => $node_wrapper->getIdentifier(),
  ];

  drush_print(dt('Migrates user @name to node ID @nid', $params));
}

drush_print('------------------');
drush_print('Done!');
