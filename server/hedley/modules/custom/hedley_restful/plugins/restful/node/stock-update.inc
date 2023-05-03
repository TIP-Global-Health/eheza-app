<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Stock Update'),
  'resource' => 'stock-update',
  'name' => 'stock-update',
  'entity_type' => 'node',
  'bundle' => 'stock_update',
  'description' => t('Exports the Stock Update bundle.'),
  'class' => 'HedleyRestfulStockUpdate',
  'authentication_types' => TRUE,
);
