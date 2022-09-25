<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Syphilis Test'),
  'resource' => 'prenatal-syphilis-test',
  'name' => 'prenatal-syphilis-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_syphilis_test',
  'description' => t('Exports the Prenatal Syphilis Test bundle.'),
  'class' => 'HedleyRestfulPrenatalSyphilisTest',
  'authentication_types' => TRUE,
);
