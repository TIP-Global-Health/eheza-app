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
        'q6-0' => 'Question 6 - 0',
        'q6-1' => 'Question 6 - 1',
        'q6-2' => 'Question 6 - 2',
        'q6-3' => 'Question 6 - 3',
        'q6-4' => 'Question 6 - 4',
        'q7-0' => 'Question 7 - 0',
        'q7-1' => 'Question 7 - 1',
        'q7-2' => 'Question 7 - 2',
        'q7-3' => 'Question 7 - 3',
        'q7-4' => 'Question 7 - 4',
        'q8-0' => 'Question 8 - 0',
        'q8-1' => 'Question 8 - 1',
        'q8-2' => 'Question 8 - 2',
        'q8-3' => 'Question 8 - 3',
        'q8-4' => 'Question 8 - 4',
        'q9-0' => 'Question 9 - 0',
        'q9-1' => 'Question 9 - 1',
        'q9-2' => 'Question 9 - 2',
        'q9-3' => 'Question 9 - 3',
        'q9-4' => 'Question 9 - 4',
        'q10-0' => 'Question 10 - 0',
        'q10-1' => 'Question 10 - 1',
        'q10-2' => 'Question 10 - 2',
        'q10-3' => 'Question 10 - 3',
        'q10-4' => 'Question 10 - 4',
        'q11-0' => 'Question 11 - 0',
        'q11-1' => 'Question 11 - 1',
        'q11-2' => 'Question 11 - 2',
        'q11-3' => 'Question 11 - 3',
        'q11-4' => 'Question 11 - 4',
        'q12-0' => 'Question 12 - 0',
        'q12-1' => 'Question 12 - 1',
        'q12-2' => 'Question 12 - 2',
        'q12-3' => 'Question 12 - 3',
        'q12-4' => 'Question 12 - 4',
        'q13-0' => 'Question 13 - 0',
        'q13-1' => 'Question 13 - 1',
        'q13-2' => 'Question 13 - 2',
        'q13-3' => 'Question 13 - 3',
        'q13-4' => 'Question 13 - 4',
        'q14-0' => 'Question 14 - 0',
        'q14-1' => 'Question 14 - 1',
        'q14-2' => 'Question 14 - 2',
        'q14-3' => 'Question 14 - 3',
        'q14-4' => 'Question 14 - 4',
        'q15-0' => 'Question 15 - 0',
        'q15-1' => 'Question 15 - 1',
        'q15-2' => 'Question 15 - 2',
        'q15-8' => 'Question 15 - 8',
        'q15-4' => 'Question 15 - 4',
        'q16-0' => 'Question 16 - 0',
        'q16-1' => 'Question 16 - 1',
        'q16-2' => 'Question 16 - 2',
        'q16-3' => 'Question 16 - 3',
        'q16-4' => 'Question 16 - 4',

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
