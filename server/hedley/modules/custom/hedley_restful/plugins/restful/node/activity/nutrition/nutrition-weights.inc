<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Weights'),
  'resource' => 'nutrition-weights',
  'name' => 'nutrition-weights',
  'entity_type' => 'node',
  'bundle' => 'nutrition_weight',
  'description' => t('Exports the Nutrition Weight bundle.'),
  'class' => 'HedleyRestfulNutritionWeights',
  'authentication_types' => TRUE,
);
