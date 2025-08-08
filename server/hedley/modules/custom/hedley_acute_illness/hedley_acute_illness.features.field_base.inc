<?php

/**
 * @file
 * hedley_acute_illness.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_acute_illness_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_114_contact'.
  $field_bases['field_114_contact'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_114_contact',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'call-114' => 'Call 114',
        'contact-site' => 'Contact recommended site',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_114_recommendation'.
  $field_bases['field_114_recommendation'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_114_recommendation',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'send-to-hc' => 'Send Patient to the nearest health center',
        'send-to-rrtc' => 'Send patient to the Rapid Response Team center',
        'send-to-hospital' => 'Send patient to the nearest hospital',
        'other' => 'Other recommendation',
        'none-no-answer' => 'None - no answer',
        'none-busy-signal' => 'None - busy Signal',
        'none-other' => 'None - other',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_abdominal_pain_period'.
  $field_bases['field_abdominal_pain_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_abdominal_pain_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_acute_illness_danger_signs'.
  $field_bases['field_acute_illness_danger_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_acute_illness_danger_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'condition-not-improving' => 'Condition not improving',
        'unable-drink-suck' => 'Unable to to drink/suck',
        'vomiting' => 'Vomiting',
        'convulsions' => 'Convulsions',
        'lethargy-unconsciousness' => 'Lethargy or unconsciousness',
        'respiratory-distress' => 'Respiratory distress',
        'spontaneous-bleeding' => 'Spontaneous bleeding',
        'bloody-diarrhea' => 'Bloody Diarrhea',
        'new-skip-rash' => 'New Skin Rash',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_acute_illness_encounter'.
  $field_bases['field_acute_illness_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_acute_illness_encounter',
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
          'acute_illness_encounter' => 'acute_illness_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_adverse_events'.
  $field_bases['field_adverse_events'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_adverse_events',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'rash-itching' => 'Rash or Itching',
        'fever' => 'Fever',
        'diarrhea' => 'Diarrhea',
        'vomiting' => 'Vomiting',
        'fatigue' => 'Fatigue',
        'other' => 'Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ambulance_arrival_time'.
  $field_bases['field_ambulance_arrival_time'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ambulance_arrival_time',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'less-than-30m' => 'Less than 30 min',
        '30m-1h' => '30 min - 1 hour',
        '1h-2h' => '1 hour- 2 hours',
        '2h-1d' => '2hours - 1 day',
        'n-a' => 'Not applicable',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_blood_in_sputum_period'.
  $field_bases['field_blood_in_sputum_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_blood_in_sputum_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_bloody_diarrhea_period'.
  $field_bases['field_bloody_diarrhea_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_bloody_diarrhea_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_body_aches_period'.
  $field_bases['field_body_aches_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_body_aches_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_chills_period'.
  $field_bases['field_chills_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_chills_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_coke_colored_urine_period'.
  $field_bases['field_coke_colored_urine_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_coke_colored_urine_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_contact_date'.
  $field_bases['field_contact_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_contact_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_contacts_trace_data'.
  $field_bases['field_contacts_trace_data'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_contacts_trace_data',
    'indexes' => array(
      'format' => array(
        0 => 'format',
      ),
    ),
    'locked' => 0,
    'module' => 'text',
    'settings' => array(
      'max_length' => 512,
    ),
    'translatable' => 0,
    'type' => 'text',
  );

  // Exported field_base: 'field_convulsions_period'.
  $field_bases['field_convulsions_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_convulsions_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_cough_period'.
  $field_bases['field_cough_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_cough_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_dry_mouth_period'.
  $field_bases['field_dry_mouth_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_dry_mouth_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_exposure'.
  $field_bases['field_exposure'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_exposure',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'covid19-symptoms' => 'Others with COVID19 symptoms',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_fever_period'.
  $field_bases['field_fever_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_fever_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_findings_signs_general'.
  $field_bases['field_findings_signs_general'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_findings_signs_general',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'lethargic-or-unconscious' => 'Lethargic or Unconscious',
        'poor-suck' => 'Poor Suck',
        'sunken-eyes' => 'Sunken Eyes',
        'poor-skin-turgor' => 'Poor Skin Turgor',
        'jaundice' => 'Jaundice',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_findings_signs_respiratory'.
  $field_bases['field_findings_signs_respiratory'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_findings_signs_respiratory',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'stridor' => 'Stridor',
        'nasal-flaring' => 'Nasal Flaring',
        'severe-wheezing' => 'Severe Wheezing',
        'sub-costal-retractions' => 'Sub-Costal Retractions',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hc_contact'.
  $field_bases['field_hc_contact'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hc_contact',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'contact-hc' => 'Contact HC',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hc_recommendation'.
  $field_bases['field_hc_recommendation'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hc_recommendation',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'send-ambulance' => 'Send ambulance',
        'home-isolation' => 'Home isolation',
        'come-to-hc' => 'Come to HC',
        'chw-monitoring' => 'CHW monitoring',
        'n-a' => 'Not applicable',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hc_response_time'.
  $field_bases['field_hc_response_time'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hc_response_time',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'less-than-30m' => 'Less than 30 min',
        '30m-1h' => '30 min - 1 hour',
        '1h-2h' => '1 hour- 2 hours',
        '2h-1d' => '2hours - 1 day',
        'n-a' => 'Not applicable',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_headache_period'.
  $field_bases['field_headache_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_headache_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_health_education_signs'.
  $field_bases['field_health_education_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_health_education_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'education-for-diagnosis' => 'Education for diagnosis',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_increased_thirst_period'.
  $field_bases['field_increased_thirst_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_increased_thirst_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_isolation'.
  $field_bases['field_isolation'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_isolation',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'patient-isolated' => 'Patient isolated',
        'sign-on-door' => 'Isolation sign on door',
        'health-education' => 'Health education',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_last_follow_up_date'.
  $field_bases['field_last_follow_up_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_last_follow_up_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_lethargy_period'.
  $field_bases['field_lethargy_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_lethargy_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_loss_of_smell_period'.
  $field_bases['field_loss_of_smell_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_loss_of_smell_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_malaria_rapid_test'.
  $field_bases['field_malaria_rapid_test'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_malaria_rapid_test',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Positive',
        'positive-and-pregnant' => 'Positive and pregnant',
        'negative' => 'Negative',
        'indeterminate' => 'Indeterminate',
        'unable-to-run' => 'Unable to run',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_missed_doses'.
  $field_bases['field_missed_doses'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_missed_doses',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_nasal_congestion_period'.
  $field_bases['field_nasal_congestion_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_nasal_congestion_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_nausea_period'.
  $field_bases['field_nausea_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_nausea_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_night_sweats_period'.
  $field_bases['field_night_sweats_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_night_sweats_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_non_administration_reason'.
  $field_bases['field_non_administration_reason'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_non_administration_reason',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'amoxicillin-lack-of-stock' => 'Amoxicillin - Lack of stock',
        'amoxicillin-known-allergy' => 'Amoxicillin - Known allergy',
        'amoxicillin-patient-declined' => 'Amoxicillin - Patient declined',
        'amoxicillin-patient-unable-to-afford' => 'Amoxicillin - Patient unable to afford',
        'amoxicillin-other' => 'Amoxicillin - Other',
        'aspirin-lack-of-stock' => 'Aspirin - Lack of stock',
        'aspirin-known-allergy' => 'Aspirin - Known allergy',
        'aspirin-patient-declined' => 'Aspirin - Patient declined',
        'aspirin-patient-unable-to-afford' => 'Aspirin - Patient unable to afford',
        'aspirin-other' => 'Aspirin - Other',
        'coartem-lack-of-stock' => 'Coartem - Lack of stock',
        'coartem-known-allergy' => 'Coartem - Known allergy',
        'coartem-patient-declined' => 'Coartem - Patient declined',
        'coartem-patient-unable-to-afford' => 'Coartem - Patient unable to afford',
        'coartem-other' => 'Coartem - Other',
        'ors-lack-of-stock' => 'ORS  - Lack of stock',
        'ors-known-allergy' => 'ORS - Known allergy',
        'ors-patient-declined' => 'ORS - Patient declined',
        'ors-patient-unable-to-afford' => 'ORS - Patient unable to afford',
        'ors-other' => 'ORS - Other',
        'zinc-lack-of-stock' => 'Zinc - Lack of stock',
        'zinc-known-allergy' => 'Zinc - Known allergy',
        'zinc-patient-declined' => 'Zinc - Patient declined',
        'zinc-patient-unable-to-afford' => 'Zinc - Patient unable to afford',
        'zinc-other' => 'Zinc - Other',
        'paracetamol-lack-of-stock' => 'Paracetamol - Lack of stock',
        'paracetamol-known-allergy' => 'Paracetamol - Known allergy',
        'paracetamol-patient-declined' => 'Paracetamol - Patient declined',
        'paracetamol-patient-unable-to-afford' => 'Paracetamol - Patient unable to afford',
        'paracetamol-other' => 'Paracetamol - Other',
        'mebendezole-lack-of-stock' => 'Mebendazole - Lack of stock',
        'mebendezole-known-allergy' => 'Mebendazole - Known allergy',
        'mebendezole-patient-declined' => 'Mebendazole - Patient declined',
        'mebendezole-patient-unable-to-afford' => 'Mebendazole - Patient unable to afford',
        'mebendezole-other' => 'Mebendazole - Other',
        'tenofovir-lack-of-stock' => 'Tenofovir - Lack of stock',
        'tenofovir-known-allergy' => 'Tenofovir - Known allergy',
        'tenofovir-patient-declined' => 'Tenofovir - Patient declined',
        'tenofovir-patient-unable-to-afford' => 'Tenofovir - Patient unable to afford',
        'tenofovir-other' => 'Tenofovir - Other',
        'lamivudine-lack-of-stock' => 'Lamivudine - Lack of stock',
        'lamivudine-known-allergy' => 'Lamivudine - Known allergy',
        'lamivudine-patient-declined' => 'Lamivudine - Patient declined',
        'lamivudine-patient-unable-to-afford' => 'Lamivudine - Patient unable to afford',
        'lamivudine-other' => 'Lamivudine - Other',
        'dolutegravir-lack-of-stock' => 'Dolutegravir - Lack of stock',
        'dolutegravir-known-allergy' => 'Dolutegravir - Known allergy',
        'dolutegravir-patient-declined' => 'Dolutegravir - Patient declined',
        'dolutegravir-patient-unable-to-afford' => 'Dolutegravir - Patient unable to afford',
        'dolutegravir-other' => 'Dolutegravir - Other',
        'tdf3tc-lack-of-stock' => 'TDF + 3TC - Lack of stock',
        'tdf3tc-known-allergy' => 'TDF + 3TC - Known allergy',
        'tdf3tc-patient-declined' => 'TDF + 3TC - Patient declined',
        'tdf3tc-patient-unable-to-afford' => 'TDF + 3TC - Patient unable to afford',
        'tdf3tc-other' => 'TDF + 3TC - Other',
        'iron-lack-of-stock' => 'Iron - Lack of stock',
        'iron-known-allergy' => 'Iron - Known allergy',
        'iron-patient-declined' => 'Iron - Patient declined',
        'iron-patient-unable-to-afford' => 'Iron - Patient unable to afford',
        'iron-other' => 'Iron - Other',
        'folicacid-lack-of-stock' => 'Folicacid - Lack of stock',
        'folicacid-known-allergy' => 'Folicacid - Known allergy',
        'folicacid-patient-declined' => 'Folicacid - Patient declined',
        'folicacid-patient-unable-to-afford' => 'Folicacid - Patient unable to afford',
        'folicacid-other' => 'Folicacid - Other',
        'ceftriaxone-lack-of-stock' => 'Ceftriaxone - Lack of stock',
        'ceftriaxone-known-allergy' => 'Ceftriaxone - Known allergy',
        'ceftriaxone-patient-declined' => 'Ceftriaxone - Patient declined',
        'ceftriaxone-patient-unable-to-afford' => 'Ceftriaxone - Patient unable to afford',
        'ceftriaxone-other' => 'Ceftriaxone - Other',
        'azithromycin-lack-of-stock' => 'Azithromycin - Lack of stock',
        'azithromycin-known-allergy' => 'Azithromycin - Known allergy',
        'azithromycin-patient-declined' => 'Azithromycin - Patient declined',
        'azithromycin-patient-unable-to-afford' => 'Azithromycin - Patient unable to afford',
        'azithromycin-other' => 'Azithromycin - Other',
        'metronidazole-lack-of-stock' => 'Metronidazole - Lack of stock',
        'metronidazole-known-allergy' => 'Metronidazole - Known allergy',
        'metronidazole-patient-declined' => 'Metronidazole - Patient declined',
        'metronidazole-patient-unable-to-afford' => 'Metronidazole - Patient unable to afford',
        'metronidazole-other' => 'Metronidazole - Other',
        'vitamina-lack-of-stock' => 'Vitamin A - Lack of stock',
        'vitamina-known-allergy' => 'Vitamin A - Known allergy',
        'vitamina-patient-declined' => 'Vitamin A - Patient declined',
        'vitamina-patient-unable-to-afford' => 'Vitamin A - Patient unable to afford',
        'vitamina-other' => 'Vitamin A - Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_non_bloody_diarrhea_period'.
  $field_bases['field_non_bloody_diarrhea_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_non_bloody_diarrhea_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_poor_suck_period'.
  $field_bases['field_poor_suck_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_poor_suck_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_prescribed_medication'.
  $field_bases['field_prescribed_medication'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prescribed_medication',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'amoxicillin' => 'Amoxicillin',
        'aspirin' => 'Aspirin',
        'coartem' => 'Coartem',
        'ors' => 'ORS',
        'zinc' => 'Zinc',
        'lemon-juice-or-honey' => 'Lemon Juice or Honey',
        'mebendezole' => 'Mebendazole',
        'paracetamol' => 'Paracetamol',
        'tenofovir' => 'Tenofovir',
        'lamivudine' => 'Lamivudine',
        'dolutegravir' => 'Dolutegravir',
        'tdf3tc' => 'TDF + 3TC',
        'iron' => 'Iron',
        'folicacid' => 'Folic Acid',
        'ceftriaxone' => 'Ceftriaxone',
        'azithromycin' => 'Azithromycin',
        'metronidazole' => 'Metronidazole',
        'vitamina' => 'Vitamin A',
        'none' => 'None of these',
        'none-initial' => 'None at initial phase',
        'none-recurrent' => 'None at recurrent phase',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_rapid_test_result'.
  $field_bases['field_rapid_test_result'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_rapid_test_result',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Positive',
        'positive-and-pregnant' => 'Positive and pregnant',
        'negative' => 'Negative',
        'unable-to-run' => 'Unable to run',
        'unable-to-run-and-pregnant' => 'Unable to run and pregnant',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_reason_for_not_isolating'.
  $field_bases['field_reason_for_not_isolating'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_reason_for_not_isolating',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'no-space' => 'No space',
        'too-ill' => 'Too ill',
        'can-not-separate' => 'Can not separate from family',
        'other' => 'Other',
        'n-a' => 'Not applicable',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_reason_for_not_taking'.
  $field_bases['field_reason_for_not_taking'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_reason_for_not_taking',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'adverse-event' => 'Adverse event',
        'no-money' => 'No money for medication',
        'memory-problems' => 'Memory problems',
        'treatment-not-started' => 'Treatment not started',
        'other' => 'Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_reason_not_given_education'.
  $field_bases['field_reason_not_given_education'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_reason_not_given_education',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'needs-emergency-referral' => 'Patient needs an emergency referral',
        'received-emergency-case' => 'Received an emergency case to treat',
        'lack-of-appropriate-education-guide' => 'Lack of appropriate education user guide',
        'patient-refused' => 'Patient refused',
        'patient-too-ill' => 'Patient too ill',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_reason_not_sent_to_hc'.
  $field_bases['field_reason_not_sent_to_hc'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_reason_not_sent_to_hc',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'client-refused' => 'Client refused',
        'no-ambulance' => 'No ambulance available',
        'unable-to-afford-fee' => 'Client unable to afford fees',
        'already-in-care' => 'Client already in care',
        'not-indicated' => 'Not indicated',
        'other' => 'Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_referred_person'.
  $field_bases['field_referred_person'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_referred_person',
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
          'person' => 'person',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_send_to_hc'.
  $field_bases['field_send_to_hc'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_send_to_hc',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'refer-to-hc' => 'Refer to HC',
        'referral-form' => 'Referral form',
        'accompany-to-hc' => 'Accompany to HC',
        'enroll-to-nutrition-program' => 'Enroll To Nutrition Program',
        'refer-to-nutrition-program' => 'Refer To Nutrition Program',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_sequence_number'.
  $field_bases['field_sequence_number'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_sequence_number',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_severe_weakness_period'.
  $field_bases['field_severe_weakness_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_severe_weakness_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_shortness_of_breath_period'.
  $field_bases['field_shortness_of_breath_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_shortness_of_breath_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_site_recommendation'.
  $field_bases['field_site_recommendation'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_site_recommendation',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'team-to-village' => 'Team will come to village',
        'send-with-form' => 'Advised to send patient to site with referral form',
        'other' => 'Other response',
        'none-sent-with-form' => 'None - no response, sent patient with referral form',
        'none-patient-refused' => 'None - patient refused',
        'none-other' => 'None - other',
        'n-a' => 'Not applicable',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_sore_throat_period'.
  $field_bases['field_sore_throat_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_sore_throat_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_spontaneos_bleeding_period'.
  $field_bases['field_spontaneos_bleeding_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_spontaneos_bleeding_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_stabbing_chest_pain_period'.
  $field_bases['field_stabbing_chest_pain_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_stabbing_chest_pain_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_symptoms_general'.
  $field_bases['field_symptoms_general'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_symptoms_general',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'fever' => 'Fever',
        'chills' => 'Chills',
        'night-sweats' => 'Night sweats',
        'body-aches' => 'Body aches',
        'headache' => 'Headache',
        'coke-colored-urine' => 'Coke colored urine',
        'convulsions' => 'Convulsions',
        'dry-mouth' => 'Dry mouth',
        'increased-thirst' => 'Increased thirst',
        'lethargy' => 'Lethargy',
        'poor-suck' => 'Poor suck',
        'severe-weakness' => 'Severe weakness',
        'spontaneos-bleeding' => 'Spontaneos bleeding',
        'unable-to-drink' => 'Unable to drink',
        'unable-to-eat' => 'Unable to eat',
        'yellow-eyes' => 'Yellow eyes',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_symptoms_gi'.
  $field_bases['field_symptoms_gi'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_symptoms_gi',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'bloody-diarrhea' => 'Bloody diarrhea',
        'non-bloody-diarrhea' => 'Non bloody diarrhea',
        'nausea' => 'Nausea',
        'vomiting' => 'Vomiting',
        'abdominal-pain' => 'Abdominal pain',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_symptoms_gi_derived_signs'.
  $field_bases['field_symptoms_gi_derived_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_symptoms_gi_derived_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'intractable-vomiting' => 'Intractable Vomiting',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_symptoms_respiratory'.
  $field_bases['field_symptoms_respiratory'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_symptoms_respiratory',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'cough' => 'Cough',
        'shortness-of-breath' => 'Shortness of breath',
        'nasal-congestion' => 'Nasal congestion',
        'blood-in-sputum' => 'Blood in sputum',
        'sore-throat' => 'Sore throat',
        'loss-of-smell' => 'Loss of smell',
        'stabbing-chest-pain' => 'Stabbing chest pain',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_trace_outcome'.
  $field_bases['field_trace_outcome'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_trace_outcome',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'no-answer' => 'No answer',
        'wrong-contact-info' => 'Wrong contact information',
        'declined-follow-up' => 'Declined Follow Up',
        'no-symptoms' => 'No Symptoms',
        'referred-to-hc' => 'Referred to HC',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_travel_history'.
  $field_bases['field_travel_history'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_travel_history',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'covid19-country' => 'COVID19 Country',
        'none' => 'None of these',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_treatment_history'.
  $field_bases['field_treatment_history'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_treatment_history',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'fever-past-six-hours' => 'Fever medication in the past 6 hours',
        'fever-past-six-hours-helped' => 'Fever medication in the past 6 hours helped',
        'malaria-today' => 'Malaria medication today',
        'malaria-today-helped' => 'Malaria medication today helped',
        'malaria-past-month' => 'Malaria medication within past month',
        'malaria-past-month-helped' => 'Malaria medication within past month helped',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_treatment_ongoing'.
  $field_bases['field_treatment_ongoing'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_treatment_ongoing',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'taken-as-prescribed' => 'Taken medication as prescribed',
        'missed-doses' => 'Missed any doses of medications',
        'feel-better' => 'Feel better after taking medications',
        'side-effects' => 'Experience side effects of the medication',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_unable_to_drink_period'.
  $field_bases['field_unable_to_drink_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_unable_to_drink_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_unable_to_eat_period'.
  $field_bases['field_unable_to_eat_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_unable_to_eat_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_vomiting_period'.
  $field_bases['field_vomiting_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_vomiting_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_yellow_eyes_period'.
  $field_bases['field_yellow_eyes_period'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_yellow_eyes_period',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  return $field_bases;
}
