<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutrition FollowUp'),
  'resource' => 'nutrition-follow-up',
  'name' => 'nutrition-follow-up',
  'entity_type' => 'node',
  'bundle' => 'nutrition_follow_up',
  'description' => t('Exports the Nutrition FollowUp bundle.'),
  'class' => 'HedleyRestfulNutritionFollowUp',
  'authentication_types' => TRUE,
);
