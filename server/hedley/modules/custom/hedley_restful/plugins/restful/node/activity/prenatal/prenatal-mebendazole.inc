<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Mebendazole'),
  'resource' => 'prenatal-mebendazole',
  'name' => 'prenatal-mebendazole',
  'entity_type' => 'node',
  'bundle' => 'prenatal_mebendazole',
  'description' => t('Exports the Prenatal Mebendazole bundle.'),
  'class' => 'HedleyRestfulPrenatalMedication',
  'authentication_types' => TRUE,
);
