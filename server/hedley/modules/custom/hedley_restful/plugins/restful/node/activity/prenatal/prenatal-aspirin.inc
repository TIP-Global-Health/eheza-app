<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Aspirin'),
  'resource' => 'prenatal-aspirin',
  'name' => 'prenatal-aspirin',
  'entity_type' => 'node',
  'bundle' => 'prenatal_aspirin',
  'description' => t('Exports the Prenatal Aspirin bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
