<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Danger Signs'),
  'resource' => 'acute-illness-danger-signs',
  'name' => 'acute-illness-danger-signs',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_danger_signs',
  'description' => t('Exports the Acute Illness Danger Signs bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessDangerSigns',
  'authentication_types' => TRUE,
);
