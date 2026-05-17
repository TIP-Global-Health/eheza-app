<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child HPV Immunisation'),
  'resource' => 'well-child-hpv-immunisation',
  'name' => 'well-child-hpv-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_hpv_immunisation',
  'description' => t('Exports the Well Child HPV Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
