<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Resources'),
  'resource' => 'resources',
  'name' => 'resources',
  'entity_type' => 'node',
  'bundle' => 'resource',
  'description' => t('Exports the Resource bundle.'),
  'class' => 'HedleyRestfulResources',
  'authentication_types' => TRUE,
);
