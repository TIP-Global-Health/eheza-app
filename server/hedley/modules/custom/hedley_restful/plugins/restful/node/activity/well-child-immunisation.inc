<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Immunisation'),
  'resource' => 'well-child-immunisation',
  'name' => 'well-child-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_immunisation',
  'description' => t('Exports the Well Child Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildImmunisation',
  'authentication_types' => TRUE,
);
