<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Oral'),
  'resource' => 'acute-illness-oral',
  'name' => 'acute-illness-oral',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_oral',
  'description' => t('Exports the Acute Illness Oral bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessOral',
  'authentication_types' => TRUE,
);
