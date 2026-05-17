<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition Health Education'),
  'resource' => 'nutrition-health-education',
  'name' => 'nutrition-health-education',
  'entity_type' => 'node',
  'bundle' => 'nutrition_health_education',
  'description' => t('Exports the Nutrition Health Education bundle.'),
  'class' => 'HedleyRestfulNutritionHealthEducation',
  'authentication_types' => TRUE,
);
