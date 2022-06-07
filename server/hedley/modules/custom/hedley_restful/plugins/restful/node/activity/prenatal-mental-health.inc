<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Mental Health'),
  'resource' => 'prenatal-mental-health',
  'name' => 'prenatal-mental-health',
  'entity_type' => 'node',
  'bundle' => 'prenatal_mental_health',
  'description' => t('Exports the Prenatal Mental Health bundle.'),
  'class' => 'HedleyRestfulPrenatalMentalHealth',
  'authentication_types' => TRUE,
);
