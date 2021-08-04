<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Health Education'),
  'resource' => 'prenatal-health-education',
  'name' => 'prenatal-health-education',
  'entity_type' => 'node',
  'bundle' => 'prenatal_health_education',
  'description' => t('Exports the Prenatal Health Education bundle.'),
  'class' => 'HedleyRestfulPrenatalHealthEducation',
  'authentication_types' => TRUE,
);
