<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Ultrasound'),
  'resource' => 'ultrasound',
  'name' => 'ultrasound',
  'entity_type' => 'node',
  'bundle' => 'ultrasound',
  'description' => t('Exports the Ultrasound bundle.'),
  'class' => 'HedleyRestfulUltrasound',
  'authentication_types' => TRUE,
);
