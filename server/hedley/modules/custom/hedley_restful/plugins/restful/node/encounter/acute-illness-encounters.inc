<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness encounters'),
  'resource' => 'acute-illness-encounters',
  'name' => 'acute-illness-encounters',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_encounter',
  'description' => t('Exports the Acute Illness Encounter bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessEncounter',
  'authentication_types' => TRUE,
);
