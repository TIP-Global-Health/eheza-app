<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Health Centers'),
  'resource' => 'health_centers',
  'name' => 'health_centers',
  'entity_type' => 'node',
  'bundle' => 'health_center',
  'description' => t('Exports the Health Center bundle.'),
  'class' => 'HedleyRestfulHealthCenters',
  'authentication_types' => TRUE,
);
