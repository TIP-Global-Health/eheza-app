<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Sessions'),
  'resource' => 'sessions',
  'name' => 'sessions',
  'entity_type' => 'node',
  'bundle' => 'session',
  'description' => t('Exports the Session bundle.'),
  'class' => 'HedleyRestfulSessions',
  'authentication_types' => TRUE,
);
