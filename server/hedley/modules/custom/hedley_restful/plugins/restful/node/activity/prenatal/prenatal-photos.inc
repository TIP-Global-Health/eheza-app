<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal photos'),
  'resource' => 'prenatal-photos',
  'name' => 'prenatal-photos',
  'entity_type' => 'node',
  'bundle' => 'prenatal_photo',
  'description' => t('Exports the Prenatal photo bundle.'),
  'class' => 'HedleyRestfulPrenatalPhotos',
  'authentication_types' => TRUE,
);
