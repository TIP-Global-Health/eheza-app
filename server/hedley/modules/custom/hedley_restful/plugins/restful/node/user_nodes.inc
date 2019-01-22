<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('User Nodes'),
  'resource' => 'user_nodes',
  'name' => 'user_nodes',
  'entity_type' => 'node',
  'bundle' => 'user',
  'description' => t('Exports the User bundle.'),
  'class' => 'HedleyRestfulUserNodes',
  'authentication_types' => TRUE,
);
