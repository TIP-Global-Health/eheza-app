<?php

/**
 * @file
 * hedley_resilience.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_resilience_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_display_day'.
  $field_bases['field_display_day'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_display_day',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_favorite_message'.
  $field_bases['field_favorite_message'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_favorite_message',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        0 => 'No',
        1 => 'Yes',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_boolean',
  );

  // Exported field_base: 'field_resilience_category'.
  $field_bases['field_resilience_category'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_resilience_category',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'guide' => 'Guide',
        'introduction' => 'Introduction',
        'growth' => 'Growth',
        'stress-management' => 'Stress Management',
        'mindfulness' => 'Mindfulness',
        'connecting' => 'Connecting',
        'self-care' => 'Self Care',
        'end-of-period' => 'End of Period',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_resilience_order'.
  $field_bases['field_resilience_order'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_resilience_order',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 4,
        5 => 5,
        6 => 6,
        7 => 7,
        8 => 8,
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_resilience_survey_signs'.
  $field_bases['field_resilience_survey_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_resilience_survey_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'q1-0' => 'Question 1 - 0',
        'q1-1' => 'Question 1 - 1',
        'q1-2' => 'Question 1 - 2',
        'q1-3' => 'Question 1 - 3',
        'q1-4' => 'Question 1 - 4',
        'q2-0' => 'Question 2 - 0',
        'q2-1' => 'Question 2 - 1',
        'q2-2' => 'Question 2 - 2',
        'q2-3' => 'Question 2 - 3',
        'q2-4' => 'Question 2 - 4',
        'q3-0' => 'Question 3 - 0',
        'q3-1' => 'Question 3 - 1',
        'q3-2' => 'Question 3 - 2',
        'q3-3' => 'Question 3 - 3',
        'q3-4' => 'Question 3 - 4',
        'q4-0' => 'Question 4 - 0',
        'q4-1' => 'Question 4 - 1',
        'q4-2' => 'Question 4 - 2',
        'q4-3' => 'Question 4 - 3',
        'q4-4' => 'Question 4 - 4',
        'q5-0' => 'Question 5 - 0',
        'q5-1' => 'Question 5 - 1',
        'q5-2' => 'Question 5 - 2',
        'q5-3' => 'Question 5 - 3',
        'q5-4' => 'Question 5 - 4',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_resilience_survey_type'.
  $field_bases['field_resilience_survey_type'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_resilience_survey_type',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'quarterly' => 'Quarterly',
        'adoption' => 'Adoption',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_time_read'.
  $field_bases['field_time_read'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_time_read',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'granularity' => array(
        'day' => 'day',
        'hour' => 'hour',
        'minute' => 'minute',
        'month' => 'month',
        'second' => 'second',
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datestamp',
  );

  return $field_bases;
}
