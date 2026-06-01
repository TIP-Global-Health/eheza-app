<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Nutritions'),
  'resource' => 'nutrition-nutritions',
  'name' => 'nutrition-nutritions',
  'entity_type' => 'node',
  'bundle' => 'nutrition_nutrition',
  'description' => t('Exports the Nutrition Nutrition bundle.'),
  'class' => 'HedleyRestfulNutritionNutritions',
  'authentication_types' => TRUE,
);
