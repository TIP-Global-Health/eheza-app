<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV encounter'),
  'resource' => 'hiv-encounter',
  'name' => 'hiv-encounter',
  'entity_type' => 'node',
  'bundle' => 'hiv_encounter',
  'description' => t('Exports the HIV Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
