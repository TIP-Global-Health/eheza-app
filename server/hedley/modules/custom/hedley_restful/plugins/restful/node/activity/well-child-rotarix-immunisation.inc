<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Rotarix Immunisation'),
  'resource' => 'well-child-rotarix-immunisation',
  'name' => 'well-child-rotarix-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_rotarix_immunisation',
  'description' => t('Exports the Well Child Rotarix Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
