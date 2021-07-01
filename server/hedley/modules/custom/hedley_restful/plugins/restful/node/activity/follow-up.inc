<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('FollowUp'),
  'resource' => 'follow-up',
  'name' => 'follow-up',
  'entity_type' => 'node',
  'bundle' => 'follow_up',
  'description' => t('Exports the FollowUp bundle.'),
  'class' => 'HedleyRestfulFollowUp',
  'authentication_types' => TRUE,
);
