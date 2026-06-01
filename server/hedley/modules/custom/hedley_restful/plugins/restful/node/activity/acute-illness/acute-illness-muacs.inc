<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Muacs'),
  'resource' => 'acute-illness-muacs',
  'name' => 'acute-illness-muacs',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_muac',
  'description' => t('Exports the Acute Illness MUAC bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessMuacs',
  'authentication_types' => TRUE,
);
