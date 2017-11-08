<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Offline Sessions'),
  'resource' => 'offline_sessions',
  'name' => 'offline_sessions',
  'entity_type' => 'node',
  'bundle' => 'session',
  'description' => t('Exports the Session bundle for use offline.'),
  'class' => 'HedleyRestfulOfflineSessions',
  'authentication_types' => TRUE,
);
