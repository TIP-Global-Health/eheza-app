<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Barcode photos'),
  'resource' => 'barcode-photos',
  'name' => 'barcode-photos',
  'entity_type' => 'node',
  'bundle' => 'barcode_photo',
  'description' => t('Exports the Barcode photo bundle.'),
  'class' => 'HedleyRestfulBarcodePhotos',
  'authentication_types' => TRUE,
);
