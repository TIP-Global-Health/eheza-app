<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Isolations'),
  'resource' => 'isolations',
  'name' => 'isolations',
  'entity_type' => 'node',
  'bundle' => 'isolation',
  'description' => t('Exports the Isolation bundle.'),
  'class' => 'HedleyRestfulIsolations',
  'authentication_types' => TRUE,
);
