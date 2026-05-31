<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Partner HIV Test'),
  'resource' => 'prenatal-partner-hiv-test',
  'name' => 'prenatal-partner-hiv-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_partner_hiv_test',
  'description' => t('Exports the Prenatal Partner HIV Test bundle.'),
  'class' => 'HedleyRestfulPrenatalPartnerHIVTest',
  'authentication_types' => TRUE,
);
