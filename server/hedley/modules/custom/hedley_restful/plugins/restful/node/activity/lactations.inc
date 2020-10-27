<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Lactations'),
  'resource' => 'lactations',
  'name' => 'lactations',
  'entity_type' => 'node',
  'bundle' => 'lactation',
  'description' => t('Exports the Lactation bundle.'),
  'class' => 'HedleyRestfulLactations',
  'authentication_types' => TRUE,
);
