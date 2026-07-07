<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Call 114s'),
  'resource' => 'call-114s',
  'name' => 'call-114s',
  'entity_type' => 'node',
  'bundle' => 'call_114',
  'description' => t('Exports the Call 114 bundle.'),
  'class' => 'HedleyRestfulCall114s',
  'authentication_types' => TRUE,
);
