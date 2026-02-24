<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition Aheza Mother'),
  'resource' => 'aheza-mother',
  'name' => 'aheza-mother',
  'entity_type' => 'node',
  'bundle' => 'aheza_mother',
  'description' => t('Exports the Aheza Mother bundle.'),
  'class' => 'HedleyRestfulFamilyNutritionAheza',
  'authentication_types' => TRUE,
);
