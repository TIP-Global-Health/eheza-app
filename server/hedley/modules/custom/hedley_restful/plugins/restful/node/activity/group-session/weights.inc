<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Weights'),
  'resource' => 'weights',
  'name' => 'weights',
  'entity_type' => 'node',
  'bundle' => 'weight',
  'description' => t('Exports the Weight bundle.'),
  'class' => 'HedleyRestfulWeights',
  'authentication_types' => TRUE,
);
