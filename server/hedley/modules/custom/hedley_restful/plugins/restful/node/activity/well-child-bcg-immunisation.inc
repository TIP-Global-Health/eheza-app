<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child BCG Immunisation'),
  'resource' => 'well-child-bcg-immunisation',
  'name' => 'well-child-bcg-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_bcg_immunisation',
  'description' => t('Exports the Well Child BCG Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
