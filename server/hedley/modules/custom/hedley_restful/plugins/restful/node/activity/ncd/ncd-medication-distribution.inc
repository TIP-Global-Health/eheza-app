<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Medication Distribution'),
  'resource' => 'ncd-medication-distribution',
  'name' => 'ncd-medication-distribution',
  'entity_type' => 'node',
  'bundle' => 'ncd_medication_distribution',
  'description' => t('Exports the NCD Medication Distribution bundle.'),
  'class' => 'HedleyRestfulNCDMedicationDistribution',
  'authentication_types' => TRUE,
);
