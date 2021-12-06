<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Barcode scans'),
  'resource' => 'barcode-scan',
  'name' => 'barcode-scan',
  'entity_type' => 'node',
  'bundle' => 'barcode_scan',
  'description' => t('Exports the Barcode scan bundle.'),
  'class' => 'HedleyRestfulBarcodeScan',
  'authentication_types' => TRUE,
);
