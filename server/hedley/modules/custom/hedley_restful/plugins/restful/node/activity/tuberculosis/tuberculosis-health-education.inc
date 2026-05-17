<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Health Education'),
  'resource' => 'tuberculosis-health-education',
  'name' => 'tuberculosis-health-education',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_health_education',
  'description' => t('Exports the Tuberculosis Health Education bundle.'),
  'class' => 'HedleyRestfulTuberculosisHealthEducation',
  'authentication_types' => TRUE,
);
