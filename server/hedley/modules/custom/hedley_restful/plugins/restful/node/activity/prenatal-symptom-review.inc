<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Symptom Review'),
  'resource' => 'prenatal-symptom-review',
  'name' => 'prenatal-symptom-review',
  'entity_type' => 'node',
  'bundle' => 'prenatal_symptom_review',
  'description' => t('Exports the Prenatal Symptom Review bundle.'),
  'class' => 'HedleyRestfulPrenatalSymptomReview',
  'authentication_types' => TRUE,
);
