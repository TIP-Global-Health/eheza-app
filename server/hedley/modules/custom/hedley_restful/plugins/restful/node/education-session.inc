<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Education Session'),
  'resource' => 'education-session',
  'name' => 'education-session',
  'entity_type' => 'node',
  'bundle' => 'education_session',
  'description' => t('Exports the Education Session bundle.'),
  'class' => 'HedleyRestfulEducationSession',
  'authentication_types' => TRUE,
);
