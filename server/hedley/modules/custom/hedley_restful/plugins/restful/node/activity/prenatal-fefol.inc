<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Fefol'),
  'resource' => 'prenatal-fefol',
  'name' => 'prenatal-fefol',
  'entity_type' => 'node',
  'bundle' => 'prenatal_fefol',
  'description' => t('Exports the Prenatal Fefol bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
