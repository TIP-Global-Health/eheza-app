<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition NCDA'),
  'resource' => 'nutrition-ncda',
  'name' => 'nutrition-ncda',
  'entity_type' => 'node',
  'bundle' => 'nutrition_ncda',
  'description' => t('Exports the Nutrition NCDA bundle.'),
  'class' => 'HedleyRestfulNutritionNCDA',
  'authentication_types' => TRUE,
);
