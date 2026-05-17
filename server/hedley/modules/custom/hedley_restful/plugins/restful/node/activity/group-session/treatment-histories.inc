<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Treatment Histories'),
  'resource' => 'treatment-histories',
  'name' => 'treatment-histories',
  'entity_type' => 'node',
  'bundle' => 'treatment_history',
  'description' => t('Exports the Treatment History bundle.'),
  'class' => 'HedleyRestfulTreatmentHistories',
  'authentication_types' => TRUE,
);
