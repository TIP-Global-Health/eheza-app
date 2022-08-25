<?php

/**
 * @file
 * hedley_ncd.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_ncd_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_ncd_danger_signs'.
  $field_bases['field_ncd_danger_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_ncd_danger_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'dyspnea' => 'Acute Shortness of Breath (Dyspnea)',
        'vision-changes' => 'Vision Changes',
        'chest-pain' => 'Chest Pain',
        'flank-pain' => 'Flank Pain',
        'hematuria' => 'Blood in Urine (Hematuria)',
        'severe-headaches' => 'Severe Headaches',
        'loss-of-conciousness' => 'Loss of Conciousness Since Last Visit',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ncd_encounter'.
  $field_bases['field_ncd_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_ncd_encounter',
    'indexes' => array(
      'target_id' => array(
        0 => 'target_id',
      ),
    ),
    'locked' => 0,
    'module' => 'entityreference',
    'settings' => array(
      'handler' => 'base',
      'handler_settings' => array(
        'behaviors' => array(
          'views-select-list' => array(
            'status' => 0,
          ),
        ),
        'sort' => array(
          'type' => 'none',
        ),
        'target_bundles' => array(
          'ncd_encounter' => 'ncd_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_ncd_group1_symptoms'.
  $field_bases['field_ncd_group1_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_ncd_group1_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'swelling-in-legs' => 'Swelling in Legs',
        'urinary-frequency' => 'Urinary Frequency',
        'anxiety' => 'Anxiety',
        'weight-loss' => 'Weight Loss',
        'palpitations' => 'Palpitations',
        'tremor' => 'Tremor',
        'swelling-in-face' => 'Swelling in Face',
        'swelling-in-abdomen' => 'Swelling in Abdomen',
        'dizziness-with-changing-position' => 'Dizziness with Changing Position',
        'mild-headache' => 'Mild Headache',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ncd_group2_symptoms'.
  $field_bases['field_ncd_group2_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_ncd_group2_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'weakness-of-one-side-of-the-body' => 'Weakness of One Side of the Body',
        'problems-with-walking' => 'Problems with Walking',
        'problems-with-talking' => 'Problems with Talking',
        'decreased-vision' => 'Decreased Vision',
        'blurry-vision' => 'lurry Vision',
        'increased-fatigue-with-daily-activities' => 'Increased Fatigue with Daily Activities',
        'short-of-breath-when-laying-down' => 'Short of Breath When Laying Down',
        'short-of-breath-at-night' => 'Short of Breath at Night',
        'kidney-problems' => 'Kidney Problems',
        'increased-thirst' => 'Increased Thirst',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ncd_pain_symptoms'.
  $field_bases['field_ncd_pain_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_ncd_pain_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'flank' => 'Flank',
        'lower-back' => 'Lower Back',
        'feet' => 'Feet',
        'neck' => 'Neck',
        'abdomen' => 'Abdomen',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
