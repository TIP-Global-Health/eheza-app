<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Caring'),
  'resource' => 'nutrition-caring',
  'name' => 'nutrition-caring',
  'entity_type' => 'node',
  'bundle' => 'nutrition_caring',
  'description' => t('Exports the NutritionCaring bundle.'),
  'class' => 'HedleyRestfulNutritionCaring',
  'authentication_types' => TRUE,
);
