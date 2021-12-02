<?php

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

require_once __DIR__ . '/HedleyAdminTextTable.php';

/**
 * Displays a textual progress bar.
 *
 * @param int $done
 *   Number of completed items.
 * @param int $total
 *   Number of total items.
 */
function progress_bar(int $done, int $total) {
  $percentage = floor(($done / $total) * 100);
  $left = 100 - $percentage;
  $write = sprintf("\033[0G\033[2K[%'={$percentage}s>%-{$left}s] - $percentage%% - $done/$total", "", "");
  fwrite(STDERR, $write);
}
