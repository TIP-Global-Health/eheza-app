<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Urine Dipstick Test'),
  'resource' => 'ncd-urine-dipstick-test',
  'name' => 'ncd-urine-dipstick-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_urine_dipstick_test',
  'description' => t('Exports the NCD Urine Dipstick Test bundle.'),
  'class' => 'HedleyRestfulNCDUrineDipstickTest',
  'authentication_types' => TRUE,
);
