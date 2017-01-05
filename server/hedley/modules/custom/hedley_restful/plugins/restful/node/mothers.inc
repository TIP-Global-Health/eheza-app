<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Mothers'),
  'resource' => 'mothers',
  'name' => 'mothers',
  'entity_type' => 'node',
  'bundle' => 'mother',
  'description' => t('Exports the Mother bundle.'),
  'class' => 'HedleyRestfulMothers',
  'authentication_types' => TRUE,
);
