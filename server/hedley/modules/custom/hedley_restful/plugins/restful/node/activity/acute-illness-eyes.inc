<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Eyes'),
  'resource' => 'acute-illness-eyes',
  'name' => 'acute-illness-eyes',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_eyes',
  'description' => t('Exports the Acute Illness Eyes bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessEyes',
  'authentication_types' => TRUE,
);
