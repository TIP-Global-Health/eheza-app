<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal medication distribution'),
  'resource' => 'prenatal-medication-distribution',
  'name' => 'prenatal-medication-distribution',
  'entity_type' => 'node',
  'bundle' => 'prenatal_medication_distribution',
  'description' => t('Exports the Prenatal Medication Distribution bundle.'),
  'class' => 'HedleyRestfulPrenatalMedicationDistribution',
  'authentication_types' => TRUE,
);
