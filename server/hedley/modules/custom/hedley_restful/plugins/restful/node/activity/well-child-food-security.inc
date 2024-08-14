<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Food Security'),
  'resource' => 'well-child-food-security',
  'name' => 'well-child-food-security',
  'entity_type' => 'node',
  'bundle' => 'well_child_food_security',
  'description' => t('Exports the Well Child Food Security bundle.'),
  'class' => 'HedleyRestfulWellChildFoodSecurity',
  'authentication_types' => TRUE,
);
