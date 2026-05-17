<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Pairing code authentication'),
  'resource' => 'pairing-code',
  'name' => 'pairing-code',
  'entity_type' => 'restful_token_auth',
  'bundle' => 'access_token',
  'description' => t('Export the pairing code authentication resource.'),
  'class' => 'HedleyRestfulPairingCode',
);
