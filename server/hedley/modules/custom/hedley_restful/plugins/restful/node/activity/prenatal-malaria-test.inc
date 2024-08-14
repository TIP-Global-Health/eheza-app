<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Malaria Test'),
  'resource' => 'prenatal-malaria-test',
  'name' => 'prenatal-malaria-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_malaria_test',
  'description' => t('Exports the Prenatal Malaria Test bundle.'),
  'class' => 'HedleyRestfulPrenatalMalariaTest',
  'authentication_types' => TRUE,
);
