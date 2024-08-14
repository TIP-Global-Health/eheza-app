<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD encounter'),
  'resource' => 'ncd-encounter',
  'name' => 'ncd-encounter',
  'entity_type' => 'node',
  'bundle' => 'ncd_encounter',
  'description' => t('Exports the NCD Encounter bundle.'),
  'class' => 'HedleyRestfulNCDEncounter',
  'authentication_types' => TRUE,
);
