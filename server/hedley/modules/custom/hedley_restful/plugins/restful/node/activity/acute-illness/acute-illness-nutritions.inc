<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Nutritions'),
  'resource' => 'acute-illness-nutritions',
  'name' => 'acute-illness-nutritions',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_nutrition',
  'description' => t('Exports the Acute Illness Nutrition bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessNutritions',
  'authentication_types' => TRUE,
);
