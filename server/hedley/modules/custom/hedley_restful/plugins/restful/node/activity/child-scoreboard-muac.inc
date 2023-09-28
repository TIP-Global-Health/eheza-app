<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Scoreboard Muac'),
  'resource' => 'child-scoreboard-muac',
  'name' => 'child-scoreboard-muac',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_muac',
  'description' => t('Exports the Child Scoreboard MUAC bundle.'),
  'class' => 'HedleyRestfulChildScoreboardMuac',
  'authentication_types' => TRUE,
);
