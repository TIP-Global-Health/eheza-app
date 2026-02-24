<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition Aheza Child'),
  'resource' => 'aheza-child',
  'name' => 'aheza-child',
  'entity_type' => 'node',
  'bundle' => 'aheza_child',
  'description' => t('Exports the Aheza Child bundle.'),
  'class' => 'HedleyRestfulFamilyNutritionAheza',
  'authentication_types' => TRUE,
);
