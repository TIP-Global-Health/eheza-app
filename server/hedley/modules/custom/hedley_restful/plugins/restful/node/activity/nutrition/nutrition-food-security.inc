<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Food Security'),
  'resource' => 'nutrition-food-security',
  'name' => 'nutrition-food-security',
  'entity_type' => 'node',
  'bundle' => 'nutrition_food_security',
  'description' => t('Exports the NutritionFoodSecurity bundle.'),
  'class' => 'HedleyRestfulNutritionFoodSecurity',
  'authentication_types' => TRUE,
);
