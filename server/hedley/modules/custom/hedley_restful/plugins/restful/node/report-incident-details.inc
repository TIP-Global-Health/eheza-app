<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Device State Report'),
  'resource' => 'report-incident-details',
  'name' => 'report-incident-details',
  'description' => t('Processes and records incident details sent by device'),
  'class' => 'HedleyRestfulReportIncidentDetails',
  'authentication_types' => ['token'],
);
