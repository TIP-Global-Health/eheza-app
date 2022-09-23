<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal HIV PCR Test'),
  'resource' => 'prenatal-hiv-pcr-test',
  'name' => 'prenatal-hiv-pcr-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_hiv_pcr_test',
  'description' => t('Exports the Prenatal HIV PCR Test bundle.'),
  'class' => 'HedleyRestfulPrenatalHIVPCRTest',
  'authentication_types' => TRUE,
);
