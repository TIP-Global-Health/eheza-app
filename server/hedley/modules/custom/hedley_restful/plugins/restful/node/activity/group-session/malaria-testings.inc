<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Malaria Testings'),
  'resource' => 'malaria-testings',
  'name' => 'malaria-testings',
  'entity_type' => 'node',
  'bundle' => 'malaria_testing',
  'description' => t('Exports the Malaria Testing bundle.'),
  'class' => 'HedleyRestfulMalariaTestings',
  'authentication_types' => TRUE,
);
