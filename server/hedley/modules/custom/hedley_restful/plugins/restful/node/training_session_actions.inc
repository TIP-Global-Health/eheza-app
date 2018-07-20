<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Training Session Actions'),
  'resource' => 'training_session_actions',
  'name' => 'training_session_actions',
  // For now, we're not actually using entities or a bundle.
  // 'entity_type' => 'node',
  // 'bundle' => '',.
  'description' => t('Creates/Deletes the training sessions.'),
  'class' => 'HedleyRestfulTrainingSessionActions',
  'authentication_types' => TRUE,
);
