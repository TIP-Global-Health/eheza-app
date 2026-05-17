<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard MR Immunisation'),
  'resource' => 'child-scoreboard-mr-immunisation',
  'name' => 'child-scoreboard-mr-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_mr_iz',
  'description' => t('Exports the Child Soreboard MR Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
