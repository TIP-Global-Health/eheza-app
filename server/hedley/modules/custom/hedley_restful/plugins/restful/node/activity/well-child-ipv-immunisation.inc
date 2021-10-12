<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child IPV Immunisation'),
  'resource' => 'well-child-ipv-immunisation',
  'name' => 'well-child-ipv-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_ipv_immunisation',
  'description' => t('Exports the Well Child IPV Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
