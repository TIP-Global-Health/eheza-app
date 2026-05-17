<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard BCG Immunisation'),
  'resource' => 'child-scoreboard-bcg-immunisation',
  'name' => 'child-scoreboard-bcg-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_bcg_iz',
  'description' => t('Exports the Child Soreboard BCG Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
