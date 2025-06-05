<?php

/**
 * @file
 * Populates field_expected_date_concluded on Individual Participant CT.
 *
 * The value set is 280 days from most recent LMP value. There can be up
 * to 2 LMP measurements for Individual Participant (represents pregnancy).
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_prenatal/scripts/populate-edd.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the number of nodes to be processed in a single chunk.
$batch = drush_get_option('batch', 50);

// Pull all prenatal participants that got NULl at EDD.
$query = db_select('node', 'n');
$query->addField('n', 'nid');
$query->leftJoin('field_data_field_expected_date_concluded', 'edd', 'edd.entity_id = n.nid');
$query->leftJoin('field_data_field_encounter_type', 'et', 'et.entity_id = n.nid');
$query->condition('et.field_encounter_type_value', 'antenatal');
$query->isNull('edd.field_expected_date_concluded_value');
$query->range(0, 10000);

$result = $query->execute()->fetchAllAssoc('nid');
if (empty($result)) {
  drush_print('There are not pregnancies that require setting EDD.');
  return;
}

$participants = array_keys($result);
$total = count($participants);
drush_print("Located $total pregnancies to process.");

$chunks = array_chunk($participants, $batch);
$count = 0;
foreach ($chunks as $ids) {
  // Free up memory.
  drupal_static_reset();

  foreach ($ids as $participant_id) {
    // Pull IDs of all encounters conducted for participant.
    $encounters = hedley_person_encounters_for_individual_participant($participant_id);
    if (empty($encounters)) {
      continue;
    }

    // Pull most recent LMP measurement.
    $query = hedley_general_create_entity_field_query_excluding_deleted();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'last_menstrual_period')
      ->fieldCondition('field_prenatal_encounter', 'target_id', $encounters, 'IN')
      ->entityOrderBy('entity_id', 'DESC')
      ->range(0, 1)
      ->execute();

    if (empty($result['node'])) {
      continue;
    }

    // Resolve LMP date.
    $lmp_id = key($result['node']);
    $wrapper = entity_metadata_wrapper('node', $lmp_id);
    $lmp_date = $wrapper->field_last_menstrual_period->value();
    // Calculate EDD date.
    $edd_date = $timestamp = strtotime('+280 days', $lmp_date);
    drush_print("Participant $participant_id has LMP with ID $lmp_id. Setting EDD to $edd_date.");

    // Set EDD date.
    $wrapper = entity_metadata_wrapper('node', $participant_id);
    $wrapper->field_expected_date_concluded->set($edd_date);
    $wrapper->save();

    $count++;
  }
}

drush_print("Done! EDD was set for total of $count pregnancies.");
