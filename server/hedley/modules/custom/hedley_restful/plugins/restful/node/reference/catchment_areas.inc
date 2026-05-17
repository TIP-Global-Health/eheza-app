<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Catchment Areas'),
  'resource' => 'catchment_areas',
  'name' => 'catchment_areas',
  'entity_type' => 'node',
  'bundle' => 'catchment_area',
  'description' => t('Exports the Catchment Area bundle.'),
  'class' => 'HedleyRestfulCatchmentAreas',
  'authentication_types' => TRUE,
);
