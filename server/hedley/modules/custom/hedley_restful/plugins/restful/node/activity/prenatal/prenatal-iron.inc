<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Iron'),
  'resource' => 'prenatal-iron',
  'name' => 'prenatal-iron',
  'entity_type' => 'node',
  'bundle' => 'prenatal_iron',
  'description' => t('Exports the Prenatal Iron bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
