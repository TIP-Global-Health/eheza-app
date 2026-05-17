<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Send to HC'),
  'resource' => 'send-to-hcs',
  'name' => 'send-to-hcs',
  'entity_type' => 'node',
  'bundle' => 'send_to_hc',
  'description' => t('Exports the Send to HC bundle.'),
  'class' => 'HedleyRestfulSendToHCs',
  'authentication_types' => TRUE,
);
