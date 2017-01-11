<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Patients'),
  'resource' => 'patients',
  'name' => 'patients',
  'entity_type' => 'node',
  // Array keyed by bundle machine, and the RESTful resource as the value.
  'bundles' => [
    'child' => 'children',
    'mother' => 'mothers',
  ],
  'description' => t('Exports all Patients (Child and Mother) bundles.'),
  'class' => 'HedleyRestfulPatients',
  'authentication_types' => TRUE,
);
