<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition Photo'),
  'resource' => 'family-nutrition-photo',
  'name' => 'family-nutrition-photo',
  'entity_type' => 'node',
  'bundle' => 'family_nutrition_photo',
  'description' => t('Exports the Family Nutrition Photo bundle.'),
  'class' => 'HedleyRestfulFamilyNutritionPhoto',
  'authentication_types' => TRUE,
);
