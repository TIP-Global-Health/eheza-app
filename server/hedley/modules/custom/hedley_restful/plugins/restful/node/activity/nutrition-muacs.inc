<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Muacs'),
  'resource' => 'nutrition-muacs',
  'name' => 'nutrition-muacs',
  'entity_type' => 'node',
  'bundle' => 'nutrition_muac',
  'description' => t('Exports the Nutrition MUAC bundle.'),
  'class' => 'HedleyRestfulNutritionMuacs',
  'authentication_types' => TRUE,
);
