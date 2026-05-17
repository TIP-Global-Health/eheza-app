<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Caring'),
  'resource' => 'well-child-caring',
  'name' => 'well-child-caring',
  'entity_type' => 'node',
  'bundle' => 'well_child_caring',
  'description' => t('Exports the Well Child Caring bundle.'),
  'class' => 'HedleyRestfulWellChildCaring',
  'authentication_types' => TRUE,
);
