<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Heights'),
  'resource' => 'nutrition-heights',
  'name' => 'nutrition-heights',
  'entity_type' => 'node',
  'bundle' => 'nutrition_height',
  'description' => t('Exports the Nutrition Height bundle.'),
  'class' => 'HedleyRestfulNutritionHeights',
  'authentication_types' => TRUE,
);
