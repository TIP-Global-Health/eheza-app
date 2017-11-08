<?php
/**
 * @file
 * hedley_schedule.features.field_instance.inc
 */

/**
 * Implements hook_field_default_field_instances().
 */
function hedley_schedule_field_default_field_instances() {
  $field_instances = array();

  // Exported field_instance: 'node-session-field_clinic'.
  $field_instances['node-session-field_clinic'] = array(
    'bundle' => 'session',
    'default_value' => NULL,
    'deleted' => 0,
    'description' => 'Enter the clinic for which data-entry is being scheduled.',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'entityreference',
        'settings' => array(
          'bypass_access' => FALSE,
          'link' => FALSE,
        ),
        'type' => 'entityreference_label',
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
    'field_name' => 'field_clinic',
    'label' => 'Clinic',
    'required' => 1,
    'settings' => array(
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'options',
      'settings' => array(),
      'type' => 'options_select',
      'weight' => -2,
    ),
  );

  // Exported field_instance: 'node-session-field_scheduled_date'.
  $field_instances['node-session-field_scheduled_date'] = array(
    'bundle' => 'session',
    'deleted' => 0,
    'description' => 'The start date is the date on which the data entry is scheduled.

The end date is the last day on which data entry will be allowed for this session through the client app (for editing, or measurements that are taken later). After this date, the session will be considered closed.',
    'display' => array(
      'default' => array(
        'label' => 'above',
        'module' => 'date',
        'settings' => array(
          'format_type' => 'long',
          'fromto' => 'both',
          'multiple_from' => '',
          'multiple_number' => '',
          'multiple_to' => '',
          'show_remaining_days' => FALSE,
        ),
        'type' => 'date_default',
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
    'field_name' => 'field_scheduled_date',
    'label' => 'Scheduled Date',
    'required' => 1,
    'settings' => array(
      'default_value' => 'now',
      'default_value2' => 'same',
      'default_value_code' => '',
      'default_value_code2' => '',
      'user_register_form' => FALSE,
    ),
    'widget' => array(
      'active' => 1,
      'module' => 'date',
      'settings' => array(
        'increment' => 15,
        'input_format' => 'Y-m-d H:i:s',
        'input_format_custom' => '',
        'label_position' => 'above',
        'no_fieldset' => 0,
        'text_parts' => array(),
        'year_range' => '-3:+3',
      ),
      'type' => 'date_select',
      'weight' => -4,
    ),
  );

  // Translatables
  // Included for use with string extractors like potx.
  t('Clinic');
  t('Enter the clinic for which data-entry is being scheduled.');
  t('Scheduled Date');
  t('The start date is the date on which the data entry is scheduled.

The end date is the last day on which data entry will be allowed for this session through the client app (for editing, or measurements that are taken later). After this date, the session will be considered closed.');

  return $field_instances;
}
