<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Vitals'),
  'resource' => 'acute-illness-vitals',
  'name' => 'acute-illness-vitals',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_vitals',
  'description' => t('Exports the Acute Illness Vitals bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessVitals',
  'authentication_types' => TRUE,
);
