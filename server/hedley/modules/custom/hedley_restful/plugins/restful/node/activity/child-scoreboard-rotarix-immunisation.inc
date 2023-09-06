<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard ROTARIX Immunisation'),
  'resource' => 'child-scoreboard-rotarix-immunisation',
  'name' => 'child-scoreboard-rotarix-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_rotarix_iz',
  'description' => t('Exports the Child Soreboard ROTARIX Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
