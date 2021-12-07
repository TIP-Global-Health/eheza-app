<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal HIV Test'),
  'resource' => 'prenatal-hiv-test',
  'name' => 'prenatal-hiv-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_hiv_test',
  'description' => t('Exports the Prenatal HIV Test bundle.'),
  'class' => 'HedleyRestfulPrenatalHIVTest',
  'authentication_types' => TRUE,
);
