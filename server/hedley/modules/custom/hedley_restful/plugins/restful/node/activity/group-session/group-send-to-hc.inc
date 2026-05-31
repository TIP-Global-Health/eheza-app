<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Group Send to HC'),
  'resource' => 'group-send-to-hc',
  'name' => 'group-send-to-hc',
  'entity_type' => 'node',
  'bundle' => 'group_send_to_hc',
  'description' => t('Exports the Group Send to HC bundle.'),
  'class' => 'HedleyRestfulGroupSendToHC',
  'authentication_types' => TRUE,
);
