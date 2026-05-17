<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Treatment Ongoing'),
  'resource' => 'treatment-ongoing',
  'name' => 'treatment-ongoing',
  'entity_type' => 'node',
  'bundle' => 'treatment_ongoing',
  'description' => t('Exports the Treatment Ongoing bundle.'),
  'class' => 'HedleyRestfulTreatmentOngoing',
  'authentication_types' => TRUE,
);
