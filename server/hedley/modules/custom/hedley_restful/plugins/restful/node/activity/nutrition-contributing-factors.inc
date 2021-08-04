<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Contributing Factors'),
  'resource' => 'nutrition-contributing-factors',
  'name' => 'nutrition-contributing-factors',
  'entity_type' => 'node',
  'bundle' => 'nutrition_contributing_factors',
  'description' => t('Exports the NutritionContributingFactors bundle.'),
  'class' => 'HedleyRestfulNutritionContributingFactors',
  'authentication_types' => TRUE,
);
