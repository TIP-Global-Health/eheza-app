<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Group Health Education'),
  'resource' => 'group-health-education',
  'name' => 'group-health-education',
  'entity_type' => 'node',
  'bundle' => 'group_health_education',
  'description' => t('Exports the Group Health Education bundle.'),
  'class' => 'HedleyRestfulGroupHealthEducation',
  'authentication_types' => TRUE,
);
