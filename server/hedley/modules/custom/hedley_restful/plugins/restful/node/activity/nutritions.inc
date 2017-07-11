<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Nutritions'),
  'resource' => 'nutritions',
  'name' => 'nutritions',
  'entity_type' => 'node',
  'bundle' => 'nutrition',
  'description' => t('Exports the Nutrition bundle.'),
  'class' => 'HedleyRestfulNutritions',
  'authentication_types' => TRUE,
);
