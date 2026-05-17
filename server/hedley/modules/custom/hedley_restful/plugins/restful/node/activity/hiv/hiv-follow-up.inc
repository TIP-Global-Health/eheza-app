<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Follow Up'),
  'resource' => 'hiv-follow-up',
  'name' => 'hiv-follow-up',
  'entity_type' => 'node',
  'bundle' => 'hiv_follow_up',
  'description' => t('Exports the HIV Follow Up bundle.'),
  'class' => 'HedleyRestfulHIVFollowUp',
  'authentication_types' => TRUE,
);
