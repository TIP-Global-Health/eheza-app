<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Symptoms'),
  'resource' => 'prenatal-symptoms',
  'name' => 'prenatal-symptoms',
  'entity_type' => 'node',
  'bundle' => 'prenatal_symptom',
  'description' => t('Exports the Prenatal Symptom bundle.'),
  'class' => 'HedleyRestfulPrenatalSymptoms',
  'authentication_types' => TRUE,
);
