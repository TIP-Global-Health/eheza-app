<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Send to HC'),
  'resource' => 'well-child-send-to-hc',
  'name' => 'well-child-send-to-hc',
  'entity_type' => 'node',
  'bundle' => 'well_child_send_to_hc',
  'description' => t('Exports the Well Child Send to HC bundle.'),
  'class' => 'HedleyRestfulWellChildSendToHC',
  'authentication_types' => TRUE,
);
