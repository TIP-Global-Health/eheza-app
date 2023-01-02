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

$key = variable_get('hedley_whatsapp_api_key', '');
if (empty($key)) {
  drush_print("API key not set. Aborting.");
  return;
}

$client = new TextClient($key);
$result = $client->SendMessage('Hello world!', 'TIP Health', [ '00972546925278' ]);

drush_print("Status code: $result->statusCode");
drush_print("Status message: $result->statusMessage");
