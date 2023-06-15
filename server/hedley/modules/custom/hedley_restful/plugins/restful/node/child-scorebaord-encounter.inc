<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scorebaord encounter'),
  'resource' => 'child-scorebaord-encounter',
  'name' => 'child-scorebaord-encounter',
  'entity_type' => 'node',
  'bundle' => 'child_scorebaord_encounter',
  'description' => t('Exports the Child Scorebaord Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
