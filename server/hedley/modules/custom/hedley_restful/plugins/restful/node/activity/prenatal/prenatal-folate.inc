<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Folate'),
  'resource' => 'prenatal-folate',
  'name' => 'prenatal-folate',
  'entity_type' => 'node',
  'bundle' => 'prenatal_folate',
  'description' => t('Exports the Prenatal Folate bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
