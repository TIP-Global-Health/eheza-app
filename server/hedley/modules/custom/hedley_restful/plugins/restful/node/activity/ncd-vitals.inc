<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Vitals'),
  'resource' => 'ncd-vitals',
  'name' => 'ncd-vitals',
  'entity_type' => 'node',
  'bundle' => 'ncd_vitals',
  'description' => t('Exports the NCD Vitals bundle.'),
  'class' => 'HedleyRestfulNCDVitals',
  'authentication_types' => TRUE,
);
