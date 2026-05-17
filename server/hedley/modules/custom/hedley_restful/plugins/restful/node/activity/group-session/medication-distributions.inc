<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Medication distributions'),
  'resource' => 'medication-distributions',
  'name' => 'medication-distributions',
  'entity_type' => 'node',
  'bundle' => 'medication_distribution',
  'description' => t('Exports the Medication Distributions bundle.'),
  'class' => 'HedleyRestfulMedicationDistributions',
  'authentication_types' => TRUE,
);
