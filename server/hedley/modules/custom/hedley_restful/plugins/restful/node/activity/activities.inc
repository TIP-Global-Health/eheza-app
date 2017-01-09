<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Activities'),
  'resource' => 'activities',
  'name' => 'activities',
  'entity_type' => 'node',
  // Array keyed by bundle machine, and the RESTful resource as the value.
  'bundles' => hedley_activity_get_activity_node_types_and_restful_name(),
  'description' => t('Exports all Activity related bundles.'),
  'class' => 'RestfulEntityBaseMultipleBundles',
  'authentication_types' => TRUE,
);
