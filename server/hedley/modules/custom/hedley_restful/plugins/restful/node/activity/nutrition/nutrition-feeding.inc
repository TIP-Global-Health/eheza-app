<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Feeding'),
  'resource' => 'nutrition-feeding',
  'name' => 'nutrition-feeding',
  'entity_type' => 'node',
  'bundle' => 'nutrition_feeding',
  'description' => t('Exports the Nutrition Feeding bundle.'),
  'class' => 'HedleyRestfulNutritionFeeding',
  'authentication_types' => TRUE,
);
