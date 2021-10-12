<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child MR Immunisation'),
  'resource' => 'well-child-mr-immunisation',
  'name' => 'well-child-mr-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_mr_immunisation',
  'description' => t('Exports the Well Child MR Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
