<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Soreboard IPV Immunisation'),
  'resource' => 'child-scoreboard-ipv-immunisation',
  'name' => 'child-scoreboard-ipv-immunisation',
  'entity_type' => 'node',
  'bundle' => 'child_scoreboard_ipv_iz',
  'description' => t('Exports the Child Soreboard IPV Immunisation bundle.'),
  'class' => 'HedleyRestfulChildScoreboardVaccination',
  'authentication_types' => TRUE,
);
