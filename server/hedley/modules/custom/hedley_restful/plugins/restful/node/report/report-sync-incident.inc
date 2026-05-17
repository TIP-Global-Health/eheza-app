<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Sync Incident Report'),
  'resource' => 'report-sync-incident',
  'name' => 'report-sync-incident',
  'description' => t('Processes and records sync incident sent by device'),
  'class' => 'HedleyRestfulReportSyncIncident',
  'authentication_types' => ['token'],
);
