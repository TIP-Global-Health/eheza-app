<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Resilience Message'),
  'resource' => 'resilience-message',
  'name' => 'resilience-message',
  'entity_type' => 'node',
  'bundle' => 'resilience_message',
  'description' => t('Exports the Resilience Message bundle.'),
  'class' => 'HedleyRestfulResilienceMessage',
  'authentication_types' => TRUE,
);
