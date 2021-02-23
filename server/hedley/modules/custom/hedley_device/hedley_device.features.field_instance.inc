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

  // Exported field_instance: 'node-sync_incident-field_content_identifier'.
  $field_instances['node-sync_incident-field_content_identifier'] = array(
    'bundle' => 'sync_incident',
    'default_value' => NULL,
    'deleted' => 0,
    'description' => '',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'text',
        'settings' => array(),
        'type' => 'text_default',
        'weight' => 3,
      ),
      'teaser' => array(
        'label' => 'above',
        'settings' => array(),
        'type' => 'hidden',
        'weight' => 0,
      ),
    ),
    'entity_type' => 'node',
    'field_name' => 'field_content_identifier',
    'label' => 'Content identifier',
    'required' => 1,
    'settings' => array(
      'text_processing' => 0,
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'text',
      'settings' => array(
        'size' => 60,
      ),
      'type' => 'text_textfield',
      'weight' => -1,
    ),
  );

  // Exported field_instance: 'node-sync_incident-field_incident_details'.
  $field_instances['node-sync_incident-field_incident_details'] = array(
    'bundle' => 'sync_incident',
    'default_value' => NULL,
    'deleted' => 0,
    'description' => '',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'text',
        'settings' => array(),
        'type' => 'text_default',
        'weight' => 2,
      ),
      'teaser' => array(
        'label' => 'above',
        'settings' => array(),
        'type' => 'hidden',
        'weight' => 0,
      ),
    ),
    'entity_type' => 'node',
    'field_name' => 'field_incident_details',
    'label' => 'Incident details',
    'required' => 0,
    'settings' => array(
      'text_processing' => 0,
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'text',
      'settings' => array(
        'rows' => 5,
      ),
      'type' => 'text_textarea',
      'weight' => -2,
    ),
  );

  // Exported field_instance: 'node-sync_incident-field_incident_type'.
  $field_instances['node-sync_incident-field_incident_type'] = array(
    'bundle' => 'sync_incident',
    'default_value' => NULL,
    'deleted' => 0,
    'description' => '',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'list',
        'settings' => array(),
        'type' => 'list_default',
        'weight' => 1,
      ),
      'teaser' => array(
        'label' => 'above',
        'settings' => array(),
        'type' => 'hidden',
        'weight' => 0,
      ),
    ),
    'entity_type' => 'node',
    'field_name' => 'field_incident_type',
    'label' => 'Incident type',
    'required' => 1,
    'settings' => array(
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'options',
      'settings' => array(),
      'type' => 'options_select',
      'weight' => -3,
    ),
  );

  // Translatables
  // Included for use with string extractors like potx.
  t('Code used to initially setup the device and start the sync process.

When creating a new device, you can leave this blank and a random pairing code will be generated.

A pairing code can only be used once -- it will be deleted when the device is paired. If it is necessary to re-pair the device, you can enter a new pairing code manually.');
  t('Content identifier');
  t('Incident details');
  t('Incident type');
  t('Pairing Code');

  return $field_instances;
}
