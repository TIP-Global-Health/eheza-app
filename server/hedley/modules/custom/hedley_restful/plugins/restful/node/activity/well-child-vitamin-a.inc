<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Vitamin A'),
  'resource' => 'well-child-vitamin-a',
  'name' => 'well-child-vitamin-a',
  'entity_type' => 'node',
  'bundle' => 'well_child_vitamin_a',
  'description' => t('Exports the Well Child Vitamin A bundle.'),
  'class' => 'HedleyRestfulWellChildVitaminA',
  'authentication_types' => TRUE,
);
