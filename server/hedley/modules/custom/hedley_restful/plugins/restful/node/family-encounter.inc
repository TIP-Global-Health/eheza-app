<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family encounters'),
  'resource' => 'family-encounters',
  'name' => 'family-encounters',
  'entity_type' => 'node',
  'bundle' => 'family_encounter',
  'description' => t('Exports the Family Encounter bundle.'),
  'class' => 'HedleyRestfulFamilyEncounter',
  'authentication_types' => TRUE,
);
