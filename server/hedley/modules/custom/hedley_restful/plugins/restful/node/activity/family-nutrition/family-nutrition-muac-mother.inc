<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition MUAC Mother'),
  'resource' => 'family-nutrition-muac-mother',
  'name' => 'family-nutrition-muac-mother',
  'entity_type' => 'node',
  'bundle' => 'family_nutrition_muac_mother',
  'description' => t('Exports the Family Nutrition MUAC Mother bundle.'),
  'class' => 'HedleyRestfulFamilyNutritionMUAC',
  'authentication_types' => TRUE,
);
