<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Medical Histories'),
  'resource' => 'medical-histories',
  'name' => 'medical-histories',
  'entity_type' => 'node',
  'bundle' => 'medical_history',
  'description' => t('Exports the Medical History bundle.'),
  'class' => 'HedleyRestfulMedicalHistories',
  'authentication_types' => TRUE,
);
