<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Urine Dipstick Test'),
  'resource' => 'prenatal-urine-dipstick-test',
  'name' => 'prenatal-urine-dipstick-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_urine_dipstick_test',
  'description' => t('Exports the Prenatal Urine Dipstick Test bundle.'),
  'class' => 'HedleyRestfulPrenatalUrineDipstickTest',
  'authentication_types' => TRUE,
);
