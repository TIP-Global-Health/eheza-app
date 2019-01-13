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
  'description' => t('Checks device pairing.'),
  'class' => 'HedleyRestfulDevices',
  // Knowing the pairing code is sufficient, without further authentication.
  'authentication_types' => [],
  'authentication_optional' => TRUE,
);
