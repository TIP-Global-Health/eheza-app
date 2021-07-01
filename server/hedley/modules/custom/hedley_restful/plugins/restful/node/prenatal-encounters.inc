<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal encounters'),
  'resource' => 'prenatal-encounters',
  'name' => 'prenatal-encounters',
  'entity_type' => 'node',
  'bundle' => 'prenatal_encounter',
  'description' => t('Exports the Prenatal Encounter bundle.'),
  'class' => 'HedleyRestfulPrenatalEncounter',
  'authentication_types' => TRUE,
);
