<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Follow Up'),
  'resource' => 'prenatal-follow-up',
  'name' => 'prenatal-follow-up',
  'entity_type' => 'node',
  'bundle' => 'prenatal_follow_up',
  'description' => t('Exports the Prenatal Follow Up bundle.'),
  'class' => 'HedleyRestfulPrenatalFollowUp',
  'authentication_types' => TRUE,
);
