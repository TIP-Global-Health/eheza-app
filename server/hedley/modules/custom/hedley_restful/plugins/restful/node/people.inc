<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('People'),
  'resource' => 'people',
  'name' => 'people',
  'entity_type' => 'node',
  'bundle' => 'person',
  'description' => t('Exports the Person bundle.'),
  'class' => 'HedleyRestfulPeople',
  'authentication_types' => TRUE,
);
