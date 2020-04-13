<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Exposures'),
  'resource' => 'exposures',
  'name' => 'exposures',
  'entity_type' => 'node',
  'bundle' => 'exposure',
  'description' => t('Exports the Exposure bundle.'),
  'class' => 'HedleyRestfulExposures',
  'authentication_types' => TRUE,
);
