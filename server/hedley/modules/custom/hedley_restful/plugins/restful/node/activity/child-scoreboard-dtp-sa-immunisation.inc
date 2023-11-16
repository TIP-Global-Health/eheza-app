<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard DTP standalone Immunisation'),
  'resource' => 'child-scoreboard-dtp-sa-immunisation',
  'name' => 'child-scoreboard-dtp-sa-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_dtp_sa_iz',
  'description' => t('Exports the Child Soreboard DTP standalone Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
