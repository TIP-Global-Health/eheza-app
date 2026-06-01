<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Treatment Review'),
  'resource' => 'tuberculosis-treatment-review',
  'name' => 'tuberculosis-treatment-review',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_treatment_review',
  'description' => t('Exports the Tuberculosis Treatment Review bundle.'),
  'class' => 'HedleyRestfulTuberculosisTreatmentReview',
  'authentication_types' => TRUE,
);
