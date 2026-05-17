<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard DTP Immunisation'),
  'resource' => 'child-scoreboard-dtp-immunisation',
  'name' => 'child-scoreboard-dtp-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_dtp_iz',
  'description' => t('Exports the Child Soreboard DTP Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
