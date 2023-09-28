<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard Height'),
  'resource' => 'child-scoreboard-height',
  'name' => 'child-scoreboard-height',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_height',
  'description' => t('Exports the Child Scoreboard Height bundle.'),
  'class' => 'HedleyRestfulChildScoreboardHeight',
  'authentication_types' => TRUE,
);
