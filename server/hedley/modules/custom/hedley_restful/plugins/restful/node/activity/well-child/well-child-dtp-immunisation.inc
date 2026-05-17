<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child DTP Immunisation'),
  'resource' => 'well-child-dtp-immunisation',
  'name' => 'well-child-dtp-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_dtp_immunisation',
  'description' => t('Exports the Well Child DTP Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
