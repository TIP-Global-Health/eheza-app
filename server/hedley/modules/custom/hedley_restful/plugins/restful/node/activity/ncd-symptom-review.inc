<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Symptom Review'),
  'resource' => 'ncd-symptom-review',
  'name' => 'ncd-symptom-review',
  'entity_type' => 'node',
  'bundle' => 'ncd_symptom_review',
  'description' => t('Exports the NCD Symptom Review bundle.'),
  'class' => 'HedleyRestfulNCDSymptomReview',
  'authentication_types' => TRUE,
);
