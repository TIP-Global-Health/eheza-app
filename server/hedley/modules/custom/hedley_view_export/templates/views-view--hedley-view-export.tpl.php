<?php

/**
 * @file
 * Main view template for AQ export.
 *
 * @var string $title
 * @var string $header
 * @var string $footer
 * @var string $rows
 *   The results of the view query, if any.
 * @var string $empty
 *   The empty text to display if the view is empty.
 * @var string $attachment_before
 * @var string $attachment_after
 *
 * Avoid indentation before prints, because it adds unwanted whitespace in the
 * csv.
 */
?>
<?php print $title; ?>
<?php print $header; ?>
<?php print $attachment_before; ?>
<?php if ($rows): ?>
<?php print $rows; ?>
<?php elseif ($empty): ?>
<?php print $empty; ?>
<?php endif; ?>
<?php print $attachment_after; ?>
<?php print $footer; ?>
