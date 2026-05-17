<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Device State Report'),
  'resource' => 'report-state',
  'name' => 'report-state',
  'description' => t('Processes and records state report sent by device'),
  'class' => 'HedleyRestfulReportState',
  'authentication_types' => ['token'],
);
