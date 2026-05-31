<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Photo'),
  'resource' => 'well-child-photo',
  'name' => 'well-child-photo',
  'entity_type' => 'node',
  'bundle' => 'well_child_photo',
  'description' => t('Exports the Well Child Photo bundle.'),
  'class' => 'HedleyRestfulWellChildPhoto',
  'authentication_types' => TRUE,
);
