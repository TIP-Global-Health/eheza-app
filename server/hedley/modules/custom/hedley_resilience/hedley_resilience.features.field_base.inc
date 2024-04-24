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
        'q6-5' => 'Question 6 - 5',
        'q6-6' => 'Question 6 - 6',
        'q6-7' => 'Question 6 - 7',
        'q6-8' => 'Question 6 - 8',
        'q6-9' => 'Question 6 - 9',
        'q7-5' => 'Question 7 - 5',
        'q7-6' => 'Question 7 - 6',
        'q7-7' => 'Question 7 - 7',
        'q7-8' => 'Question 7 - 8',
        'q7-9' => 'Question 7 - 9',
        'q8-5' => 'Question 8 - 5',
        'q8-6' => 'Question 8 - 6',
        'q8-7' => 'Question 8 - 7',
        'q8-8' => 'Question 8 - 8',
        'q8-9' => 'Question 8 - 9',
        'q9-5' => 'Question 9 - 5',
        'q9-6' => 'Question 9 - 6',
        'q9-7' => 'Question 9 - 7',
        'q9-8' => 'Question 9 - 8',
        'q9-9' => 'Question 9 - 9',
        'q10-5' => 'Question 10 - 5',
        'q10-6' => 'Question 10 - 6',
        'q10-7' => 'Question 10 - 7',
        'q10-8' => 'Question 10 - 8',
        'q10-9' => 'Question 10 - 9',
        'q11-5' => 'Question 11 - 5',
        'q11-6' => 'Question 11 - 6',
        'q11-7' => 'Question 11 - 7',
        'q11-8' => 'Question 11 - 8',
        'q11-9' => 'Question 11 - 9',
        'q12-5' => 'Question 12 - 5',
        'q12-6' => 'Question 12 - 6',
        'q12-7' => 'Question 12 - 7',
        'q12-8' => 'Question 12 - 8',
        'q12-9' => 'Question 12 - 9',
        'q13-5' => 'Question 13 - 5',
        'q13-6' => 'Question 13 - 6',
        'q13-7' => 'Question 13 - 7',
        'q13-8' => 'Question 13 - 8',
        'q13-9' => 'Question 13 - 9',
        'q14-5' => 'Question 14 - 5',
        'q14-6' => 'Question 14 - 6',
        'q14-7' => 'Question 14 - 7',
        'q14-8' => 'Question 14 - 8',
        'q14-9' => 'Question 14 - 9',
        'q15-5' => 'Question 15 - 5',
        'q15-6' => 'Question 15 - 6',
        'q15-7' => 'Question 15 - 7',
        'q15-8' => 'Question 15 - 8',
        'q15-9' => 'Question 15 - 9',
        'q16-5' => 'Question 16 - 5',
        'q16-6' => 'Question 16 - 6',
        'q16-7' => 'Question 16 - 7',
        'q16-8' => 'Question 16 - 8',
        'q16-9' => 'Question 16 - 9',

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
