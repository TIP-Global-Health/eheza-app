<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Height'),
  'resource' => 'well-child-height',
  'name' => 'well-child-height',
  'entity_type' => 'node',
  'bundle' => 'well_child_height',
  'description' => t('Exports the Well Child Height bundle.'),
  'class' => 'HedleyRestfulWellChildHeight',
  'authentication_types' => TRUE,
);
