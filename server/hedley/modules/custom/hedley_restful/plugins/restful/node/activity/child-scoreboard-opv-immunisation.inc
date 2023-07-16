<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard OPV Immunisation'),
  'resource' => 'child-scoreboard-opv-immunisation',
  'name' => 'child-scoreboard-opv-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_opv_iz',
  'description' => t('Exports the Child Soreboard OPV Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
