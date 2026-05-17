<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('WhatsApp Record'),
  'resource' => 'whatsapp-record',
  'name' => 'whatsapp-record',
  'entity_type' => 'node',
  'bundle' => 'whatsapp_record',
  'description' => t('Exports the WhatsApp Record bundle.'),
  'class' => 'HedleyRestfulWhatsAppRecord',
  'authentication_types' => TRUE,
);
