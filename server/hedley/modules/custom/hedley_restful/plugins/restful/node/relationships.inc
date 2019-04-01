<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Relationships'),
  'resource' => 'relationships',
  'name' => 'relationships',
  'entity_type' => 'node',
  'bundle' => 'relationship',
  'description' => t('Exports the Relationship bundle.'),
  'class' => 'HedleyRestfulRelationships',
  'authentication_types' => TRUE,
);
