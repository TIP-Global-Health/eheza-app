<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Villages'),
  'resource' => 'villages',
  'name' => 'villages',
  'entity_type' => 'node',
  'bundle' => 'village',
  'description' => t('Exports the Village bundle.'),
  'class' => 'HedleyRestfulVillages',
  'authentication_types' => TRUE,
);
