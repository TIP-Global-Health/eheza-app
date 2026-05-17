<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Hygiene'),
  'resource' => 'nutrition-hygiene',
  'name' => 'nutrition-hygiene',
  'entity_type' => 'node',
  'bundle' => 'nutrition_hygiene',
  'description' => t('Exports the NutritionHygiene bundle.'),
  'class' => 'HedleyRestfulNutritionHygiene',
  'authentication_types' => TRUE,
);
