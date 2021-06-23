<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Health Education'),
  'resource' => 'well-child-health-education',
  'name' => 'well-child-health-education',
  'entity_type' => 'node',
  'bundle' => 'well_child_health_education',
  'description' => t('Exports the WellChild Health Education bundle.'),
  'class' => 'HedleyRestfulWellChildHealthEducation',
  'authentication_types' => TRUE,
);
