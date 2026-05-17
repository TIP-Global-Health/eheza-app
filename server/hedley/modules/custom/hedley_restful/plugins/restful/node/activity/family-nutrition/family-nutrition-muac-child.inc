<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition MUAC Child'),
  'resource' => 'family-nutrition-muac-child',
  'name' => 'family-nutrition-muac-child',
  'entity_type' => 'node',
  'bundle' => 'family_nutrition_muac_child',
  'description' => t('Exports the Family Nutrition MUAC Child bundle.'),
  'class' => 'HedleyRestfulFamilyNutritionMUAC',
  'authentication_types' => TRUE,
);
