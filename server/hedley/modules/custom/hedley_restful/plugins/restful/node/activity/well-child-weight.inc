<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Weight'),
  'resource' => 'well-child-weight',
  'name' => 'well-child-weight',
  'entity_type' => 'node',
  'bundle' => 'well_child_weight',
  'description' => t('Exports the WellChild Weight bundle.'),
  'class' => 'HedleyRestfulWellChildWeight',
  'authentication_types' => TRUE,
);
