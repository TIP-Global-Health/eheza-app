<?php

/**
 * @file
 * Template to display a view as a csv table.
 *
 * @var array $header
 *   Header labels keyed by field id. Notice it gets removed by
 *   hedley_view_export_export_batch_worker(), except for in the first batch.
 * @var array $rows
 *   An array of row items. Each row is an array of content.
 *   $rows are keyed by row number, fields within rows are keyed by field ID.
 *
 * Don't add indentation before prints, as it can change how the csv is parsed.
 */
?>
<?php hedley_view_export_print_csv($header); ?>
<?php array_map('hedley_view_export_print_csv', $rows); ?>
