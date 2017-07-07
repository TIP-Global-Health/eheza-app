<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Photos'),
  'resource' => 'photos',
  'name' => 'photos',
  'entity_type' => 'node',
  'bundle' => 'photo',
  'description' => t('Exports the Photo bundle.'),
  'class' => 'HedleyRestfulPhotos',
  'authentication_types' => TRUE,
);
