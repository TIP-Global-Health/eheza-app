<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Hemoglobin Test'),
  'resource' => 'prenatal-hemoglobin-test',
  'name' => 'prenatal-hemoglobin-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_hemoglobin_test',
  'description' => t('Exports the Prenatal Hemoglobin Test bundle.'),
  'class' => 'HedleyRestfulPrenatalHemoglobinTest',
  'authentication_types' => TRUE,
);
