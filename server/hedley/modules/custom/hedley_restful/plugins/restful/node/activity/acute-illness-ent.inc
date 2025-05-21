<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness ENT'),
  'resource' => 'acute-illness-ent',
  'name' => 'acute-illness-ent',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_ent',
  'description' => t('Exports the Acute Illness ENT bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessENT',
  'authentication_types' => TRUE,
);
