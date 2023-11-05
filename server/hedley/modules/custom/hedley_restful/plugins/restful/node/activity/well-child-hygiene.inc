<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Hygiene'),
  'resource' => 'well-child-hygiene',
  'name' => 'well-child-hygiene',
  'entity_type' => 'node',
  'bundle' => 'well_child_hygiene',
  'description' => t('Exports the Well Child Hygiene bundle.'),
  'class' => 'HedleyRestfulWellChildHygiene',
  'authentication_types' => TRUE,
);
