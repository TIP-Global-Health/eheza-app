<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Travel Histories'),
  'resource' => 'travel-histories',
  'name' => 'travel-histories',
  'entity_type' => 'node',
  'bundle' => 'travel_history',
  'description' => t('Exports the Travel History bundle.'),
  'class' => 'HedleyRestfulTravelHistories',
  'authentication_types' => TRUE,
);
