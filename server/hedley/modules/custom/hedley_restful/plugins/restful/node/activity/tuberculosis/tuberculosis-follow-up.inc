<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Follow Up'),
  'resource' => 'tuberculosis-follow-up',
  'name' => 'tuberculosis-follow-up',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_follow_up',
  'description' => t('Exports the Tuberculosis Follow Up bundle.'),
  'class' => 'HedleyRestfulTuberculosisFollowUp',
  'authentication_types' => TRUE,
);
