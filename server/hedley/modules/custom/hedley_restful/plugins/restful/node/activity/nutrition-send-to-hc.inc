<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Send to HC'),
  'resource' => 'nutrition-send-to-hc',
  'name' => 'nutrition-send-to-hc',
  'entity_type' => 'node',
  'bundle' => 'nutrition_send_to_hc',
  'description' => t('Exports the Nutrition Send to HC bundle.'),
  'class' => 'HedleyRestfulNutritionSendToHC',
  'authentication_types' => TRUE,
);
s
