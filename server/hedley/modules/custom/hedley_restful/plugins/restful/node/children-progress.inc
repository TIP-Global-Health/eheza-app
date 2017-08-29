<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Children Progress'),
  'resource' => 'children-progress',
  'name' => 'children-progress',
  'entity_type' => 'node',
  'bundle' => 'child',
  'description' => t('Exports Progress of a Child.'),
  'class' => 'HedleyRestfulChildrenProgress',
  'authentication_types' => TRUE,
);
