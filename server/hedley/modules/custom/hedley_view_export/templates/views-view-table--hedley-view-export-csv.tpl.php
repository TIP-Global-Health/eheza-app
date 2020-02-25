<?php

/**
 * @file
 * Template to display a view as a csv table.
 *
 * @var string $title
 *   The title of this group of rows.  May be empty.
 * @var array $header
 *   Header labels keyed by field id.
 * @var string $caption
 *   The caption for this table. May be empty.
 * @var array $header_classes
 *   Header classes keyed by field id.
 * @var string $fields
 *   CSS IDs to use for each field id.
 * @var array $rows
 *   An array of row items. Each row is an array of content.
 *   $rows are keyed by row number, fields within rows are keyed by field ID.
 * @var array $field_classes
 *   Classes to apply to each field, indexed by field id, then row number.
 *   This matches the index in $rows.
 *
 * Don't add indentation before prints, as it can change how the csv is parsed.
 */
?>
<?php if (!empty($title) || !empty($caption)): ?>
<?php print $caption . $title; ?>
<?php endif; ?>
<?php hedley_view_export_print_csv($header); ?>
<?php array_map('hedley_view_export_print_csv', $rows); ?>
<?php print "\n"; ?>
