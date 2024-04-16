<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Feeding'),
  'resource' => 'well-child-feeding',
  'name' => 'well-child-feeding',
  'entity_type' => 'node',
  'bundle' => 'well_child_feeding',
  'description' => t('Exports the Well Child Feeding bundle.'),
  'class' => 'HedleyRestfulWellChildFeeding',
  'authentication_types' => TRUE,
);
