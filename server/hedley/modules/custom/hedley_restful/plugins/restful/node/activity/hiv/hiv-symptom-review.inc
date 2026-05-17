<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Symptom Review'),
  'resource' => 'hiv-symptom-review',
  'name' => 'hiv-symptom-review',
  'entity_type' => 'node',
  'bundle' => 'hiv_symptom_review',
  'description' => t('Exports the HIV Symptom Review bundle.'),
  'class' => 'HedleyRestfulHIVSymptomReview',
  'authentication_types' => TRUE,
);
