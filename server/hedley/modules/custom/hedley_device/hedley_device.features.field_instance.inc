<?php

/**
 * @file
 * hedley_device.features.field_instance.inc
 */

/**
 * Implements hook_field_default_field_instances().
 */
function hedley_device_field_default_field_instances() {
  $field_instances = array();

  // Exported field_instance: 'node-device-field_pairing_code'.
  $field_instances['node-device-field_pairing_code'] = array(
    'bundle' => 'device',
    'default_value' => NULL,
    'deleted' => 0,
    'description' => 'Code used to initially setup the device and start the sync process.

When creating a new device, you can leave this blank and a random pairing code will be generated.

A pairing code can only be used once -- it will be deleted when the device is paired. If it is necessary to re-pair the device, you can enter a new pairing code manually.',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'text',
        'settings' => array(),
        'type' => 'text_default',
        'weight' => 0,
      ),
      'teaser' => array(
        'label' => 'above',
        'settings' => array(),
        'type' => 'hidden',
        'weight' => 0,
      ),
    ),
    'entity_type' => 'node',
    'field_name' => 'field_pairing_code',
    'label' => 'Pairing Code',
    'required' => 0,
    'settings' => array(
      'text_processing' => 0,
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'text',
      'settings' => array(
        'size' => 8,
      ),
      'type' => 'text_textfield',
      'weight' => -4,
    ),
  );

  // Translatables
  // Included for use with string extractors like potx.
  t('Code used to initially setup the device and start the sync process.

When creating a new device, you can leave this blank and a random pairing code will be generated.

A pairing code can only be used once -- it will be deleted when the device is paired. If it is necessary to re-pair the device, you can enter a new pairing code manually.');
  t('Pairing Code');

  return $field_instances;
}
