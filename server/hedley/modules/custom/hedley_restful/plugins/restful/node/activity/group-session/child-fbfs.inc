<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Child Fbfs'),
  'resource' => 'child-fbfs',
  'name' => 'child-fbfs',
  'entity_type' => 'node',
  'bundle' => 'child_fbf',
  'description' => t('Exports the Child FBF bundle.'),
  'class' => 'HedleyRestfulChildFBFs',
  'authentication_types' => TRUE,
);
