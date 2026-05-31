<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition encounters'),
  'resource' => 'nutrition-encounters',
  'name' => 'nutrition-encounters',
  'entity_type' => 'node',
  'bundle' => 'nutrition_encounter',
  'description' => t('Exports the Nutrition Encounter bundle.'),
  'class' => 'HedleyRestfulNutritionEncounter',
  'authentication_types' => TRUE,
);
