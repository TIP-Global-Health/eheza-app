<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Nutrition encounter'),
  'resource' => 'family-nutrition-encounter',
  'name' => 'family-nutrition-encounter',
  'entity_type' => 'node',
  'bundle' => 'family_nutrition_encounter',
  'description' => t('Exports the Family Nutrition Encounter bundle.'),
  'class' => 'HedleyRestfulFamilyEncounter',
  'authentication_types' => TRUE,
);
