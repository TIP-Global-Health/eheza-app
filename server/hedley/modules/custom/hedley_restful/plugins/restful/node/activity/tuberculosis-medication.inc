<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Medication'),
  'resource' => 'tuberculosis-medication',
  'name' => 'tuberculosis-medication',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_medication',
  'description' => t('Exports the Tuberculosis Medication bundle.'),
  'class' => 'HedleyRestfulTuberculosisMedication',
  'authentication_types' => TRUE,
);
