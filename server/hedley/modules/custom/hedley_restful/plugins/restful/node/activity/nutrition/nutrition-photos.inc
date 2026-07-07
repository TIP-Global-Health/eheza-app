<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Photos'),
  'resource' => 'nutrition-photos',
  'name' => 'nutrition-photos',
  'entity_type' => 'node',
  'bundle' => 'nutrition_photo',
  'description' => t('Exports the Nutrition Photo bundle.'),
  'class' => 'HedleyRestfulNutritionPhotos',
  'authentication_types' => TRUE,
);
