<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Albendazole'),
  'resource' => 'well-child-albendazole',
  'name' => 'well-child-albendazole',
  'entity_type' => 'node',
  'bundle' => 'well_child_albendazole',
  'description' => t('Exports the Well Child Albendazole bundle.'),
  'class' => 'HedleyRestfulWellChildMedication',
  'authentication_types' => TRUE,
);
