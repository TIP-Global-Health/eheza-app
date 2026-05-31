<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Home Visit encounters'),
  'resource' => 'home-visit-encounter',
  'name' => 'home-visit-encounter',
  'entity_type' => 'node',
  'bundle' => 'home_visit_encounter',
  'description' => t('Exports the Home Visit Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
