<?php

/**
 * @file
 * Delta export script — exports only changes since a given revision ID.
 *
 * Usage:
 *   drush scr export-delta.php --since-vid=12345 --site=rwanda
 *   terminus drush site.env -- scr .../export-delta.php --since-vid=12345
 *
 * Options:
 *   --since-vid  Required. Process revisions with vid > this value.
 *   --site       Site identifier: 'rwanda' (default) or 'burundi'.
 */

// Prevent the well-child script from running its full export.
define('HEDLEY_DELTA_EXPORT', TRUE);

require_once __DIR__ . '/../lib/export-framework.php';
require_once __DIR__ . '/export-well-child-research.php';
require_once __DIR__ . '/../lib/delta-exporter.php';

$last_vid = drush_get_option('since-vid', NULL);
if ($last_vid === NULL) {
  drush_set_error('MISSING_OPTION', 'Required option --since-vid not provided.');
  exit(1);
}

$last_vid = (int) $last_vid;
if ($last_vid < 0) {
  drush_set_error('INVALID_OPTION', '--since-vid must be a non-negative integer.');
  exit(1);
}

$site = drush_get_option('site', 'rwanda');
if (!in_array($site, ['rwanda', 'burundi'])) {
  drush_set_error('INVALID_OPTION', "--site must be 'rwanda' or 'burundi'.");
  exit(1);
}

$exporter = new HedleyMigrateDeltaExporter($site);
$exporter->exportDelta($last_vid);
