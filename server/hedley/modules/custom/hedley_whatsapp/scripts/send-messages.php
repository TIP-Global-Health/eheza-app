<?php

/**
 * @file
 * Delivers WhatsApp records to messaging vendor.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_whatsapp/scripts/send-messages.php.
 */

use Twilio\Rest\Client;
use Twilio\Exceptions\TwilioException;
use Twilio\Exceptions\ConfigurationException;

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$twilio_sid = variable_get('hedley_whatsapp_twilio_sid', '');
if (empty($twilio_sid)) {
  drush_print("Twilio account SID not set. Aborting.");
  return;
}

$twilio_token = variable_get('hedley_whatsapp_twilio_token', '');
if (empty($twilio_token)) {
  drush_print("Twilio token not set. Aborting.");
  return;
}

$twilio_messaging_service_sid = variable_get('hedley_whatsapp_twilio_messaging_service_sid', '');
if (empty($twilio_token)) {
  drush_print("Twilio messaging service SID not set. Aborting.");
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get the allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$type = 'whatsapp_record';
// Maximal number of attempts of delivering message.
$delivery_attempts = variable_get('hedley_whatsapp_delivery_attempts', 5);

$base_query = hedley_general_create_db_select_query_excluding_deleted();
$base_query->addField('n', 'nid');
$base_query->condition('n.status', NODE_PUBLISHED);
$base_query->condition('n.type', $type);
$base_query->leftJoin('field_data_field_date_concluded', 'dc', 'n.nid = dc.entity_id');
$base_query->isNull('dc.field_date_concluded_value');
$base_query->leftJoin('field_data_field_delivery_attempts', 'da', 'n.nid = da.entity_id');
$base_query->condition('da.field_delivery_attempts_value', $delivery_attempts, '<');

$count_query = clone $base_query;
if ($nid) {
  $count_query->condition('n.nid', $nid, '>');
}
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no $type messages to deliver.");
  return;
}

drush_print("Located $total $type messages for delivery.");

try {
  $twilio = new Client($twilio_sid, $twilio_token);
}
catch (ConfigurationException $e) {
  drush_print('Failed to load Twilio client.');
  return;
}

$processed = 0;
$template_sid = '';
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->condition('nid', $nid, '>');
  }

  $ids = $query
    ->range(0, $batch)
    ->execute()
    ->fetchCol();

  if (empty($ids)) {
    // No more items left.
    break;
  }

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $wrapper = entity_metadata_wrapper('node', $node);
    $phone_number = $wrapper->field_phone_number->value();
    if (empty($phone_number)) {
      drush_print("Failed to pull destination number for node ID $node->nid. Giving up on it.");
      continue;
    }

    $fid = $node->field_screenshot[LANGUAGE_NONE][0]['fid'];
    if (empty($fid)) {
      drush_print("Failed to pull the file for node ID $node->nid. Giving up on it.");
      continue;
    }
    $file = file_load($fid);
    // Copy file to public repository, so it can be
    // fetched by Twilio without authentication.
    $copy = file_copy($file, 'public://' . $file->filename, FILE_EXISTS_REPLACE);
    $image_uri = file_create_url($copy->uri);
    $parsed_uri = parse_url($image_uri);

    $language = $wrapper->field_language->value();
    $template_sid = hedley_whatsapp_get_progress_report_template_sid($language);
    if (empty($template_sid)) {
      drush_print("Failed to pull message template SID for node ID $node->nid. Giving up on it.");
      continue;
    }

    $report_type = $wrapper->field_report_type->value();

    $date = date('d-m-Y', $wrapper->field_date_measured->value());

    $patient_id = $wrapper->field_person->getIdentifier();
    $wrapper_patient = entity_metadata_wrapper('node', $patient_id);
    $first_name = trim($wrapper_patient->field_first_name->value());
    $second_name = trim($wrapper_patient->field_second_name->value());
    if (empty($first_name) && empty($second_name)) {
      $second_name = $wrapper->label();
    }
    $patient_name = "$second_name $first_name";

    drush_print('Forwarding message to vendor...');
    try {
      $result = $twilio->messages
        ->create("whatsapp:$phone_number", [
          "contentSid" => $template_sid,
          "from" => $twilio_messaging_service_sid,
          "contentVariables" => json_encode([
            "1" => $report_type,
            "2" => $patient_name,
            "3" => $date,
            "4" => substr($parsed_uri['path'], 1),
          ]),
        ]);

      $wrapper->field_date_concluded->set(time());
      $wrapper->save();
    }
    catch (TwilioException $e) {
      $attempts = $wrapper->field_delivery_attempts->value();
      $wrapper->field_delivery_attempts->set($attempts + 1);
      $wrapper->save();
    }
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($nodes);
  $processed += $count;
  drush_print("$count $type messages processed.");
}

drush_print("Done! Total of $processed $type messages processed.");
