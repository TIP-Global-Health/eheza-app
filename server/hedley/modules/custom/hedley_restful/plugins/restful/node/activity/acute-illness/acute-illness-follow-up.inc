<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness FollowUp'),
  'resource' => 'acute-illness-follow-up',
  'name' => 'acute-illness-follow-up',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_follow_up',
  'description' => t('Exports the AcuteIllnessFollowUp bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessFollowUp',
  'authentication_types' => TRUE,
);
