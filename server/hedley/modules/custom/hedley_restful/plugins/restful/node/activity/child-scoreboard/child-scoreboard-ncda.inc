<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard NCDA'),
  'resource' => 'child-scoreboard-ncda',
  'name' => 'child-scoreboard-ncda',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_ncda',
  'description' => t('Exports the Child Scoreboard NCDA bundle.'),
  'class' => 'HedleyRestfulChildScoreboardNCDA',
  'authentication_types' => TRUE,
);
