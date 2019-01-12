<?php

/**
 * @file
 * Authenticate with device tokens.
 */

$plugin = [
  'label' => t('Authenticatie with a device token'),
  'description' => t('Authenticate requests based on a device token sent in the request.'),
  'name' => 'device-token',
  'class' => 'HedleyRestfulDeviceToken',
  'options' => [
    'param_name' => 'access_token',
  ],
];
