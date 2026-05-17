<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child DTP standalone Immunisation'),
  'resource' => 'well-child-dtp-sa-immunisation',
  'name' => 'well-child-dtp-sa-immunisation',
  'entity_type' => 'node',
  'bundle' => 'well_child_dtp_sa_immunisation',
  'description' => t('Exports the Well Child DTP standalone Immunisation bundle.'),
  'class' => 'HedleyRestfulWellChildVaccination',
  'authentication_types' => TRUE,
);
