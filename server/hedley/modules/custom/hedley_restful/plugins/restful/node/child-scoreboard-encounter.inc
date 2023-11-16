<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard encounter'),
  'resource' => 'child-scoreboard-encounter',
  'name' => 'child-scoreboard-encounter',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_encounter',
  'description' => t('Exports the Child Scoreboard Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
