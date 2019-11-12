<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Device Sync'),
  'resource' => 'sync',
  'name' => 'sync',
  'description' => t('Sync with devices.'),
  'class' => 'HedleyRestfulSync',
  // @todo: Why was just token?
  'authentication_types' => TRUE,
);
