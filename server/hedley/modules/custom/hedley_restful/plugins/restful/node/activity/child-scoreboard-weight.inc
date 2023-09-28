<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard Weight'),
  'resource' => 'child-scoreboard-weight',
  'name' => 'child-scoreboard-weight',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_weight',
  'description' => t('Exports the Child Scoreboard Weight bundle.'),
  'class' => 'HedleyRestfulChildScoreboardWeight',
  'authentication_types' => TRUE,
);
