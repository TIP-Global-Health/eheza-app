<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Reports Data for Elm App'),
  'resource' => 'reports-data',
  'name' => 'reports-data',
  'description' => t('Produces data for reports'),
  'class' => 'HedleyRestfulReportsData',
  'authentication_types' => ['token'],
);
