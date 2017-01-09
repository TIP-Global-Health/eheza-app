<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Heights'),
  'resource' => 'heights',
  'name' => 'heights',
  'entity_type' => 'node',
  'bundle' => 'height',
  'description' => t('Exports the Height bundle.'),
  'class' => 'HedleyRestfulHeights',
  'authentication_types' => TRUE,
);
