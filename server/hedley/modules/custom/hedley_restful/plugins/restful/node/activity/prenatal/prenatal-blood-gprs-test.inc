<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Blood GpRs Test'),
  'resource' => 'prenatal-blood-gprs-test',
  'name' => 'prenatal-blood-gprs-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_blood_gprs_test',
  'description' => t('Exports the Prenatal Blood GpRs Test bundle.'),
  'class' => 'HedleyRestfulPrenatalBloodGpRsTest',
  'authentication_types' => TRUE,
);
