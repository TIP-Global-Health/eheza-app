<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Health Education'),
  'resource' => 'health-education',
  'name' => 'health-education',
  'entity_type' => 'node',
  'bundle' => 'health_education',
  'description' => t('Exports the Health Education bundle.'),
  'class' => 'HedleyRestfulHealthEducation',
  'authentication_types' => TRUE,
);
