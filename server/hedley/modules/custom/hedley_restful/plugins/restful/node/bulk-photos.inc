<?php

/**
 * @file
 * Restful plugin: bulk photo fetch.
 */

$plugin = array(
  'label' => t('Bulk Photo Fetch'),
  'resource' => 'bulk-photos',
  'name' => 'bulk-photos',
  'description' => t('Return many styled photos in a single binary response.'),
  'class' => 'HedleyRestfulBulkPhotos',
  'authentication_types' => ['token'],
);
