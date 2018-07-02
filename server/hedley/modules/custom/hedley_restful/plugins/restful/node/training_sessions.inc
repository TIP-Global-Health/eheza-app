<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Training Sessions'),
  'resource' => 'training_sessions',
  'name' => 'training_sessions',
  'entity_type' => 'node',
  'bundle' => 'session',
  'description' => t('Creates/Deletes the training sessions.'),
  'class' => 'HedleyRestfulTrainingSessions',
  'authentication_types' => TRUE,
);
