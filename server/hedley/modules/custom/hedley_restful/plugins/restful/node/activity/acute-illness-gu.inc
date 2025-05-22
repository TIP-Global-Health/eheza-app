<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness GU'),
  'resource' => 'acute-illness-gu',
  'name' => 'acute-illness-gu',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_gu',
  'description' => t('Exports the Acute Illness GU bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessGU',
  'authentication_types' => TRUE,
);
