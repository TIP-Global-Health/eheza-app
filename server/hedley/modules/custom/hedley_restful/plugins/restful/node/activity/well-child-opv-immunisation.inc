<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child OPV Immunisation'),
  'resource' => 'well-child-opv-immunisation',
  'name' => 'well-child-opv-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_opv_immunisation',
  'description' => t('Exports the Well Child OPV Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
