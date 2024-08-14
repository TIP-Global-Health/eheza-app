<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Health Education'),
  'resource' => 'hiv-health-education',
  'name' => 'hiv-health-education',
  'entity_type' => 'node',
  'bundle' => 'hiv_health_education',
  'description' => t('Exports the HIV Health Education bundle.'),
  'class' => 'HedleyRestfulHIVHealthEducation',
  'authentication_types' => TRUE,
);
