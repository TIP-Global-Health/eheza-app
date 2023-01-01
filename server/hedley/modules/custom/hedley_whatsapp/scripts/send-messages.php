<?php

/**
 * @file
 * Locates duplicates by UUID and deletes all, but first one.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_whatsapp/scripts/send-messages.php.
 */

use CMText\TextClient;

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$client = new TextClient('cac034a9-0c8b-47dd-b1d2-55cb23fd5349');
$result = $client->SendMessage('Hello world!', 'TIP Health', [ '00972546925278' ]);

drush_print("Status code: $result->statusCode");
drush_print("Status message: $result->statusMessage");
