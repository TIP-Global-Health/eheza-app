<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Medications'),
  'resource' => 'medications',
  'name' => 'medications',
  'entity_type' => 'node',
  'bundle' => 'medication',
  'description' => t('Exports the Medications bundle.'),
  'class' => 'HedleyRestfulMedications',
  'authentication_types' => TRUE,
);
