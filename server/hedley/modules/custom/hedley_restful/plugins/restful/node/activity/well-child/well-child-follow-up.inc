<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child FollowUp'),
  'resource' => 'well-child-follow-up',
  'name' => 'well-child-follow-up',
  'entity_type' => 'node',
  'bundle' => 'well_child_follow_up',
  'description' => t('Exports the Well Child Follow Up bundle.'),
  'class' => 'HedleyRestfulWellChildFollowUp',
  'authentication_types' => TRUE,
);
