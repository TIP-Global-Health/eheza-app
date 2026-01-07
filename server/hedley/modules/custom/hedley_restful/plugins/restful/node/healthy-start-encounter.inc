<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Healthy Start encounter'),
  'resource' => 'healthy-start-encounter',
  'name' => 'healthy-start-encounter',
  'entity_type' => 'node',
  'bundle' => 'healthy_start_encounter',
  'description' => t('Exports the Healthy Start Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
