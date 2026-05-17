<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Symptom Review'),
  'resource' => 'tuberculosis-symptom-review',
  'name' => 'tuberculosis-symptom-review',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_symptom_review',
  'description' => t('Exports the Tuberculosis Symptom Review bundle.'),
  'class' => 'HedleyRestfulTuberculosisSymptomReview',
  'authentication_types' => TRUE,
);
