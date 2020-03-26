<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Vitals'),
  'resource' => 'vitals',
  'name' => 'vitals',
  'entity_type' => 'node',
  'bundle' => 'vitals',
  'description' => t('Exports the Vitals bundle.'),
  'class' => 'HedleyRestfulVitals',
  'authentication_types' => TRUE,
);
