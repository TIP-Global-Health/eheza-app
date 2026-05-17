<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child ECD'),
  'resource' => 'well-child-ecd',
  'name' => 'well-child-ecd',
  'entity_type' => 'node',
  'bundle' => 'well_child_ecd',
  'description' => t('Exports the Well Child ECD bundle.'),
  'class' => 'HedleyRestfulWellChildEcd',
  'authentication_types' => TRUE,
);
