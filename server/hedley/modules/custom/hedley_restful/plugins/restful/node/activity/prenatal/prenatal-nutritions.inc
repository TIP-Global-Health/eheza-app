<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Nutritions'),
  'resource' => 'prenatal-nutritions',
  'name' => 'prenatal-nutritions',
  'entity_type' => 'node',
  'bundle' => 'prenatal_nutrition',
  'description' => t('Exports the Prenatal Nutrition bundle.'),
  'class' => 'HedleyRestfulPrenatalNutritions',
  'authentication_types' => TRUE,
);
