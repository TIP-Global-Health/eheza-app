<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Devices'),
  'resource' => 'devices',
  'name' => 'devices',
  'entity_type' => 'node',
  'bundle' => 'device',
  'description' => t('Exports the Device bundle.'),
  'class' => 'HedleyRestfulDevices',
  'authentication_types' => TRUE,
);
