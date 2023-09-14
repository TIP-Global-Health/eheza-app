<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard PCV13 Immunisation'),
  'resource' => 'child-scoreboard-pcv13-immunisation',
  'name' => 'child-scoreboard-pcv13-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_pcv13_iz',
  'description' => t('Exports the Child Soreboard PCV13 Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
