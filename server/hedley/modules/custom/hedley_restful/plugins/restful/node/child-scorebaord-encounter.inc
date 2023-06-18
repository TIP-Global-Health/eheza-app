<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scorebaord encounter'),
  'resource' => 'child-scoreboard-encounter',
  'name' => 'child-scoreboard-encounter',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_encounter',
  'description' => t('Exports the Child Scorebaord Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
