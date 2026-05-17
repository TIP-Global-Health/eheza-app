<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Medication History'),
  'resource' => 'ncd-medication-history',
  'name' => 'ncd-medication-history',
  'entity_type' => 'node',
  'bundle' => 'ncd_medication_history',
  'description' => t('Exports the NCD Medication History bundle.'),
  'class' => 'HedleyRestfulNCDMedicationHistory',
  'authentication_types' => TRUE,
);
