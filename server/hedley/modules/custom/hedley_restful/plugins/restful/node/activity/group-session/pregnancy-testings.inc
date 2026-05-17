<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Pregnancy Testings'),
  'resource' => 'pregnancy-testings',
  'name' => 'pregnancy-testings',
  'entity_type' => 'node',
  'bundle' => 'pregnancy_testing',
  'description' => t('Exports the Pregnancy Testing bundle.'),
  'class' => 'HedleyRestfulPregnancyTesting',
  'authentication_types' => TRUE,
);
