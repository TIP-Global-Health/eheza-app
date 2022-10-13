<?php

/**
 * @file
 * Generate Hedley demo content via Drush.
 */

const HEDLEY_MIGRATE_HEALTH_CENTER_NYANGE = 7089;

/**
 * Hook for defining drush commands.
 */
function hedley_migrate_drush_command() {
  $items['migrate-nurse-to-nurse-content-type'] = [
    'callback' => 'hedley_migrate_nurses_to_content_type',
    'description' => 'Create a nurse content type for each user entity',
    'arguments' => [],
    'options' => [],
  ];
  // Command for creating fake nurses and devices.
  $items['create-dummy-nurses-devices'] = [
    'callback' => 'hedley_migrate_create_dummy_nurses_devices',
    'description' => 'Create fake nurses and devices',
    'arguments' => [],
    'options' => [],
  ];

  return $items;
}

/**
 * Creates fake nurses and devices.
 */
function hedley_migrate_create_dummy_nurses_devices($amount = 100) {
  $pins = [];
  $pairing_codes = [];
  for ($i = 0; $i < $amount; $i++) {
    $pin = hedley_user_get_unique_pincode();
    $pairing_code = hedley_device_get_unique_pairing_code();
    $pins[] = $pin;
    $pairing_codes[] = $pairing_code;
    $device = entity_create('node', [
      'type' => 'device',
      'title' => 'Device ' . $pairing_code,
      'field_pairing_code' => [
        LANGUAGE_NONE => [
          0 => [
            'value' => $pairing_code,
          ],
        ],
      ],
    ]);
    node_save($device);
    $nurse = entity_create('node', [
      'type' => 'nurse',
      'title' => 'Nurse ' . $pin,
      'field_pin_code' => [
        LANGUAGE_NONE => [
          [
            'value' => $pin,
          ],
        ],
      ],
      'field_health_centers' => [
        LANGUAGE_NONE => [
          [
            'target_id' => HEDLEY_MIGRATE_HEALTH_CENTER_NYANGE,
          ],
        ],
      ],
    ]);
    node_save($nurse);
  }
  file_put_contents('pins.txt', implode("\n", $pins));
  file_put_contents('pairing_codes.txt', implode("\n", $pairing_codes));
}

/**
 * Create a nurse entity for every user.
 */
function hedley_migrate_nurses_to_content_type() {
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
}
