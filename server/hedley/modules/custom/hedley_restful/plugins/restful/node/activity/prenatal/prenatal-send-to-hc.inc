<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Send to HC'),
  'resource' => 'prenatal-send-to-hc',
  'name' => 'prenatal-send-to-hc',
  'entity_type' => 'node',
  'bundle' => 'prenatal_send_to_hc',
  'description' => t('Exports the PrenatalSendToHc bundle.'),
  'class' => 'HedleyRestfulPrenatalSendToHC',
  'authentication_types' => TRUE,
);
