<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Anc Send to HC'),
  'resource' => 'anc-send-to-hc',
  'name' => 'anc-send-to-hc',
  'entity_type' => 'node',
  'bundle' => 'anc_send_to_hc',
  'description' => t('Exports the AncSendToHc bundle.'),
  'class' => 'HedleyRestfulPrenatalSendToHC',
  'authentication_types' => TRUE,
);
