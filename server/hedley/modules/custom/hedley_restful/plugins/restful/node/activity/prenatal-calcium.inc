<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Calcium'),
  'resource' => 'prenatal-calcium',
  'name' => 'prenatal-calcium',
  'entity_type' => 'node',
  'bundle' => 'prenatal_calcium',
  'description' => t('Exports the Prenatal Calcium bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
