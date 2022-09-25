<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Hepatitis B Test'),
  'resource' => 'prenatal-hepatitis-b-test',
  'name' => 'prenatal-hepatitis-b-test',
  'entity_type' => 'node',
  'bundle' => 'prenatal_hepatitis_b_test',
  'description' => t('Exports the Prenatal Hepatitis B Test bundle.'),
  'class' => 'HedleyRestfulPrenatalHepatitisBTest',
  'authentication_types' => TRUE,
);
