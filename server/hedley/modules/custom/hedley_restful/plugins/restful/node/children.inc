<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Children'),
  'resource' => 'children',
  'name' => 'children',
  'entity_type' => 'node',
  'bundle' => 'child',
  'description' => t('Exports the Child bundle.'),
  'class' => 'HedleyRestfulChildren',
  'authentication_types' => TRUE,
);
