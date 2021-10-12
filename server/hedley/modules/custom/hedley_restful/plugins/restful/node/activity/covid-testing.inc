<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Covid Testing'),
  'resource' => 'covid-testing',
  'name' => 'covid-testing',
  'entity_type' => 'node',
  'bundle' => 'covid_testing',
  'description' => t('Exports the Covid Testing bundle.'),
  'class' => 'HedleyRestfulCovidTesting',
  'authentication_types' => TRUE,
);
