<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Mother Fbfs'),
  'resource' => 'mother-fbfs',
  'name' => 'mother-fbfs',
  'entity_type' => 'node',
  'bundle' => 'mother_fbf',
  'description' => t('Exports the Mother FBF bundle.'),
  'class' => 'HedleyRestfulMotherFBFs',
  'authentication_types' => TRUE,
);
