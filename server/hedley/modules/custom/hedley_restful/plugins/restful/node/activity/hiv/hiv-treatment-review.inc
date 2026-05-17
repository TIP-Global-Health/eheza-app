<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Treatment Review'),
  'resource' => 'hiv-treatment-review',
  'name' => 'hiv-treatment-review',
  'entity_type' => 'node',
  'bundle' => 'hiv_treatment_review',
  'description' => t('Exports the HIV Treatment Review bundle.'),
  'class' => 'HedleyRestfulHIVTreatmentReview',
  'authentication_types' => TRUE,
);
