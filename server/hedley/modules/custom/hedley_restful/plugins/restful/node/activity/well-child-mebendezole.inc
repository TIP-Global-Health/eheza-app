<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Mebendezole'),
  'resource' => 'well-child-mebendezole',
  'name' => 'well-child-mebendezole',
  'entity_type' => 'node',
  'bundle' => 'well_child_mebendezole',
  'description' => t('Exports the Well Child Mebendezole bundle.'),
  'class' => 'HedleyRestfulWellChildMebendezole',
  'authentication_types' => TRUE,
);
