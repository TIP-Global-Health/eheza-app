<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis encounter'),
  'resource' => 'tuberculosis-encounter',
  'name' => 'tuberculosis-encounter',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_encounter',
  'description' => t('Exports the Tuberculosis Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
