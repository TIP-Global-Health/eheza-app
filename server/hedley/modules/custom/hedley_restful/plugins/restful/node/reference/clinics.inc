<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Clinics'),
  'resource' => 'clinics',
  'name' => 'clinics',
  'entity_type' => 'node',
  'bundle' => 'clinic',
  'description' => t('Exports the Clinic bundle.'),
  'class' => 'HedleyRestfulClinics',
  'authentication_types' => TRUE,
);
