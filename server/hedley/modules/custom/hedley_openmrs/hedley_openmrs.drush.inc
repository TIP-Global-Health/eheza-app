<?php

/**
 * @file
 * Drush commands for the Hedley OpenMRS integration.
 */

/**
 * Implements hook_drush_command().
 */
function hedley_openmrs_drush_command() {
  $items['hedley-openmrs-build-prenatal-catalog'] = [
    'description' => 'Generate the prenatal concept catalog (field -> OpenMRS concept) as JSON.',
    'aliases' => ['ho-catalog'],
    'callback' => 'hedley_openmrs_drush_build_prenatal_catalog',
  ];

  return $items;
}

/**
 * Prenatal measurement bundles whose fields become OpenMRS observations.
 *
 * The prenatal_encounter bundle itself is excluded — it becomes the
 * Encounter, not an observation.
 */
function _hedley_openmrs_prenatal_bundles() {
  return [
    'appointment_confirmation', 'prenatal_aspirin', 'prenatal_blood_gprs_test',
    'prenatal_breastfeeding', 'prenatal_calcium', 'prenatal_fefol',
    'prenatal_folate', 'prenatal_follow_up', 'prenatal_gu_exam',
    'prenatal_hemoglobin_test', 'prenatal_hepatitis_b_test',
    'prenatal_hiv_pcr_test', 'prenatal_hiv_test', 'prenatal_iron',
    'prenatal_labs_results', 'prenatal_malaria_test', 'prenatal_mebendazole',
    'prenatal_medication_distribution', 'prenatal_mental_health',
    'prenatal_mms', 'prenatal_outside_care', 'prenatal_partner_hiv_test',
    'prenatal_random_blood_sugar_test', 'prenatal_send_to_hc',
    'prenatal_speciality_care', 'prenatal_symptom_review',
    'prenatal_syphilis_test', 'prenatal_tetanus_immunisation',
    'prenatal_ultrasound', 'prenatal_urine_dipstick_test', 'vitals',
  ];
}

/**
 * Fields never mapped to an observation (operational / reference fields).
 */
function _hedley_openmrs_skip_fields() {
  return [
    'field_prenatal_encounter', 'field_uuid', 'field_shard', 'field_nurse',
    'field_person', 'field_deleted', 'field_health_center',
  ];
}

/**
 * Maps a Drupal field type to an OpenMRS concept datatype.
 */
function _hedley_openmrs_concept_datatype($drupal_type) {
  $numeric = ['number_integer', 'number_float', 'number_decimal'];
  $date = ['datetime', 'date', 'datestamp'];
  if (in_array($drupal_type, $numeric, TRUE)) {
    return 'Numeric';
  }
  if (in_array($drupal_type, $date, TRUE)) {
    return 'Date';
  }
  if ($drupal_type === 'list_boolean') {
    return 'Boolean';
  }
  return 'Text';
}

/**
 * Builds a human-readable concept name from a field key.
 *
 * "field_hemoglobin_count" -> "E-Heza Hemoglobin Count".
 */
function _hedley_openmrs_concept_name($field_name) {
  $base = preg_replace('/^field_/', '', $field_name);
  return 'E-Heza ' . ucwords(str_replace('_', ' ', $base));
}

/**
 * Callback: build the prenatal concept catalog and write it to JSON.
 */
function hedley_openmrs_drush_build_prenatal_catalog() {
  $skip = _hedley_openmrs_skip_fields();
  $rows = [];
  $seen = [];

  foreach (_hedley_openmrs_prenatal_bundles() as $bundle) {
    $instances = field_info_instances('node', $bundle);
    foreach ($instances as $field_name => $instance) {
      if (in_array($field_name, $skip, TRUE) || isset($seen[$field_name])) {
        continue;
      }
      $field = field_info_field($field_name);
      if (empty($field)) {
        continue;
      }
      $seen[$field_name] = TRUE;
      $key = preg_replace('/^field_/', '', $field_name);
      $rows[] = [
        'eheza_key' => $key,
        'concept_name' => _hedley_openmrs_concept_name($field_name),
        'datatype' => _hedley_openmrs_concept_datatype($field['type']),
        'measurement' => $bundle,
      ];
    }
  }

  // Stable order so the committed file has a deterministic diff.
  usort($rows, function ($a, $b) {
    return strcmp($a['eheza_key'], $b['eheza_key']);
  });

  $path = DRUPAL_ROOT . '/../../integration/openmrs/prenatal-concept-catalog.json';
  $json = json_encode($rows, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
  file_put_contents($path, $json . "\n");

  drush_print(sprintf('Wrote %d catalog rows to %s', count($rows), $path));
}
