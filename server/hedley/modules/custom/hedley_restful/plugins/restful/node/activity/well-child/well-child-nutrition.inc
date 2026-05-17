<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Nutrition'),
  'resource' => 'well-child-nutrition',
  'name' => 'well-child-nutrition',
  'entity_type' => 'node',
  'bundle' => 'well_child_nutrition',
  'description' => t('Exports the Well Child Nutrition bundle.'),
  'class' => 'HedleyRestfulWellChildNutrition',
  'authentication_types' => TRUE,
);
