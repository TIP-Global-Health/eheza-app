<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal MMS'),
  'resource' => 'prenatal-mms',
  'name' => 'prenatal-mms',
  'entity_type' => 'node',
  'bundle' => 'prenatal_mms',
  'description' => t('Exports the Prenatal MMS bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
