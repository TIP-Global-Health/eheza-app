<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Medication'),
  'resource' => 'hiv-medication',
  'name' => 'hiv-medication',
  'entity_type' => 'node',
  'bundle' => 'hiv_medication',
  'description' => t('Exports the HIV Medication bundle.'),
  'class' => 'HedleyRestfulHIVMedication',
  'authentication_types' => TRUE,
);
