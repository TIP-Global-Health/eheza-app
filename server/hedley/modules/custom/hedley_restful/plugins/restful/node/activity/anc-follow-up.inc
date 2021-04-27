<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('ANC Follow Up'),
  'resource' => 'anc-follow-up',
  'name' => 'anc-follow-up',
  'entity_type' => 'node',
  'bundle' => 'anc_follow_up',
  'description' => t('Exports the AncFollowUp bundle.'),
  'class' => 'HedleyRestfulPrenatalFollowUp',
  'authentication_types' => TRUE,
);
