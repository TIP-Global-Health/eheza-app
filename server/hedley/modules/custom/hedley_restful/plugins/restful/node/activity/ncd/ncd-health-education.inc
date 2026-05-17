<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Health Education'),
  'resource' => 'ncd-health-education',
  'name' => 'ncd-health-education',
  'entity_type' => 'node',
  'bundle' => 'ncd_health_education',
  'description' => t('Exports the NCD Health Education bundle.'),
  'class' => 'HedleyRestfulNCDHealthEducation',
  'authentication_types' => TRUE,
);
