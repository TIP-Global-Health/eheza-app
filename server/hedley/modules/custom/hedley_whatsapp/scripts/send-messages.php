<?php

/**
 * @file
 * Delivers WhatsApp records to messaging vendor.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_whatsapp/scripts/send-messages.php.
 */

use CMText\TextClient;
use CMText\Message;
use CMText\Channels;
use CMText\RichContent\Messages\TemplateMessage;
use CMText\RichContent\Templates\Whatsapp\WhatsappTemplate;
use CMText\RichContent\Templates\Whatsapp\Language;
use CMText\RichContent\Templates\Whatsapp\ComponentHeader;
use CMText\RichContent\Templates\Whatsapp\ComponentBody;
use CMText\RichContent\Templates\Whatsapp\ComponentParameterImage;
use CMText\RichContent\Templates\Whatsapp\ComponentParameterText;
use CMText\RichContent\Templates\Whatsapp\ComponentParameterDatetime;
use CMText\RichContent\Messages\MediaContent;

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
// todo: delete this.
//$result = $client->SendMessage('Hi!', 'TIP Health', [ '00972546925278' ]);
// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get the allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

$type = 'whatsapp_record';

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

drush_print("$total nodes of type $type located.");

$processed = 0;
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '>');
  }

  $result = $query
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    // No more items left.
    break;
  }

  $messages = [];
  $ids = array_keys($result['node']);
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $wrapper = entity_metadata_wrapper('node', $node);
    $phone_number = $wrapper->field_phone_number->value();
    if (empty($phone_number)) {
      continue;
    }
    $phone_number = '00972546925278';

    $fid = $node->field_screenshot[LANGUAGE_NONE][0]['fid'];
    if (empty($fid)) {
      continue;
    }
    $file = file_load($fid);

    $report_type = $wrapper->field_report_type->value();
    $date = date('d-m-Y', $wrapper->field_date_measured->value());
    $datetime = DateTime::createFromFormat('!d-m-Y', $date, new DateTimeZone("UTC"));

    $patient_id = $wrapper->field_person->getIdentifier();
    $wrapper_patient = entity_metadata_wrapper('node', $patient_id);
    $first_name = trim($wrapper_patient->field_first_name->value());
    $second_name = trim($wrapper_patient->field_second_name->value());
    if (empty($first_name) && empty($second_name)) {
      $second_name = $wrapper->label();
    }
    $patient_name = "$second_name $first_name";

    try {
      $message = new Message('', 'TIP Health', [$phone_number]);

      $image_uri = file_create_url($file->uri);
      if (strpos($image_uri, 'http://') === 0) {
        $image_uri = str_replace('http://', 'https://', $image_uri);
      }

      drush_print("Image: $image_uri");

      $header_param =
        new ComponentParameterImage(
          new MediaContent(
            $file->filename,
            $image_uri,
            $file->filemime
          )
        );

      $body_param1 = new ComponentParameterText($report_type);
      $body_param2 = new ComponentParameterText($patient_name);
      $body_param3 = new ComponentParameterDatetime('', $datetime);

      // todo: need to set language ?
      $message
        ->WithChannels([Channels::WHATSAPP])
        ->WithTemplate(
          new TemplateMessage(
            new WhatsappTemplate(
              'progress_report',
              'b88ec70e_692b_4c85_a6e1_1117fc7d69b7',
              new Language('en'),
              [
                new ComponentHeader([$header_param]),
                new ComponentBody([$body_param1, $body_param2, $body_param3]),
              ]
            )
          )
        );
      $messages[] = $message;
    }
    catch (Exception $exception) {
      $attempts = $wrapper->field_delivery_attempts->value();
      $wrapper->field_delivery_attempts->set($attempts + 1);
      $wrapper->save();
    }
  }

  $result = $client->send($messages);
  // todo: process response.
  drush_print("Status code: $result->statusCode");
  drush_print("Status message: $result->statusMessage");
  drush_print("Details: $result->details");

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($nodes);
  $processed += $count;
  drush_print("$count nodes of type $type processed.");
}

drush_print("Done! $processed WhatsApp records processed.");
