<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child PCV13 Immunisation'),
  'resource' => 'well-child-pcv13-immunisation',
  'name' => 'well-child-pcv13-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_pcv13_immunisation',
  'description' => t('Exports the Well Child PCV13 Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
