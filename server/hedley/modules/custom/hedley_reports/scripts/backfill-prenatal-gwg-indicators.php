<?php

/**
 * @file
 * Backfills the GWG indicator on existing prenatal encounters.
 *
 * Mirrors the Elm classification (Pages.Prenatal.Activity.Utils:
 * resolveGWGClassification / resolveGWGClassificationForHealthyStart)
 * and stamps each encounter's field_prenatal_indicators with either
 * 'adequate-gwg' or 'inadequate-gwg' (preserving existing values such
 * as 'past-labs-completed').
 *
 * The Healthy Start variant of the algorithm is used when the
 * 'hedley_admin_feature_healthy_start_enabled' variable is truthy.
 *
 * Idempotent: any prior 'adequate-gwg'/'inadequate-gwg' on an encounter
 * is removed before the new verdict is written, so re-running the
 * script after tweaking the math is safe.
 *
 * Encounters where the verdict cannot be computed (no current weight,
 * no LMP, no pre-pregnancy weight, no first encounter for Healthy Start,
 * no MUAC and no BMI for Healthy Start, etc.) are skipped -- their
 * field_prenatal_indicators is left untouched.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/backfill-prenatal-gwg-indicators.php
 *   [--nid=N] [--batch=50] [--memory_limit=800] [--dry-run]
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$nid = drush_get_option('nid', 0);
$batch = drush_get_option('batch', 50);
$memory_limit = drush_get_option('memory_limit', 800);
$dry_run = (bool) drush_get_option('dry-run', FALSE);

$is_healthy_start = (bool) variable_get('hedley_admin_feature_healthy_start_enabled', FALSE);

// Standard-path weight-gain standards
// (kg in first trimester, kg per week thereafter).
// Mirrors weightGainStandardsPerPrePregnancyClassification.
$std_standards = [
  'under-weight' => ['first_trimester' => 2.0, 'per_week' => 0.51],
  'normal'       => ['first_trimester' => 1.0, 'per_week' => 0.42],
  'over-weight'  => ['first_trimester' => 1.0, 'per_week' => 0.28],
  'obesity'      => ['first_trimester' => 0.5, 'per_week' => 0.22],
];
// Healthy-Start per-day standards (kg/day before 13w, kg/day from 13w on).
// Mirrors weightGainStandardsByPrePregnancyClassificationHealthyStart.
$hs_standards = [
  'under-weight' => ['before_13w' => 0.0235, 'from_13w' => 0.073],
  'normal'       => ['before_13w' => 0.0235, 'from_13w' => 0.060],
  'over-weight'  => ['before_13w' => 0.0235, 'from_13w' => 0.060],
  'obesity'      => ['before_13w' => 0.0235, 'from_13w' => 0.060],
];

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
$base_query
  ->entityCondition('entity_type', 'node')
  ->entityCondition('bundle', 'individual_participant')
  ->fieldCondition('field_encounter_type', 'value', 'antenatal')
  ->propertyCondition('status', NODE_PUBLISHED);

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print('No antenatal pregnancies to backfill.');
  return;
}

drush_print("Healthy Start mode: " . ($is_healthy_start ? 'YES' : 'NO'));
drush_print("Dry run: " . ($dry_run ? 'YES' : 'NO'));
drush_print("$count antenatal pregnancies to scan.");

$total_pregnancies = 0;
$total_set = 0;
$total_unchanged = 0;
$total_skipped = 0;
$skip_reasons = [];

while (TRUE) {
  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '>');
  }

  $result = $query
    ->propertyOrderBy('nid', 'ASC')
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    break;
  }

  $ids = array_keys($result['node']);
  foreach ($ids as $participant_id) {
    $stats = backfill_gwg_for_pregnancy(
      $participant_id,
      $is_healthy_start,
      $dry_run,
      $std_standards,
      $hs_standards
    );
    $total_pregnancies++;
    $total_set += $stats['set'];
    $total_unchanged += $stats['unchanged'];
    $total_skipped += $stats['skipped'];
    foreach ($stats['skip_reasons'] as $reason => $cnt) {
      $skip_reasons[$reason] = ($skip_reasons[$reason] ?? 0) + $cnt;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Resume with --nid=@nid', ['@nid' => $nid]));
    return;
  }

  drush_print("Pregnancies: $total_pregnancies | encounters set: $total_set | unchanged: $total_unchanged | skipped: $total_skipped | mem: ${memory}MB");

  drupal_static_reset();
  $nid = end($ids);
}

drush_print('');
drush_print('=== Backfill complete ===');
drush_print("Pregnancies processed:   $total_pregnancies");
drush_print("Encounters indicator set:  $total_set");
drush_print("Encounters unchanged:      $total_unchanged");
drush_print("Encounters skipped:        $total_skipped");
if (!empty($skip_reasons)) {
  drush_print('Skip reasons:');
  foreach ($skip_reasons as $reason => $cnt) {
    drush_print("  - $reason: $cnt");
  }
}

/**
 * Computes and writes the GWG indicator for every encounter in a pregnancy.
 *
 * @param int $participant_id
 *   The individual_participant node ID (antenatal type).
 * @param bool $is_healthy_start
 *   Whether to use the Healthy Start algorithm.
 * @param bool $dry_run
 *   When TRUE, classifies but does not save.
 * @param array $std_standards
 *   Standard-path weight-gain standards, keyed by classification.
 * @param array $hs_standards
 *   Healthy-Start per-day standards, keyed by classification.
 *
 * @return array
 *   ['set' => int, 'unchanged' => int, 'skipped' => int,
 *    'skip_reasons' => [reason => count]].
 */
function backfill_gwg_for_pregnancy($participant_id, $is_healthy_start, $dry_run, array $std_standards, array $hs_standards) {
  $stats = ['set' => 0, 'unchanged' => 0, 'skipped' => 0, 'skip_reasons' => []];

  $participant = node_load($participant_id);
  if (!$participant) {
    return $stats;
  }

  $person_id = $participant->field_person[LANGUAGE_NONE][0]['target_id'] ?? NULL;
  if (!$person_id) {
    return $stats;
  }
  $person = node_load($person_id);
  $birth_date = $person && !empty($person->field_birth_date[LANGUAGE_NONE][0]['value'])
    ? substr($person->field_birth_date[LANGUAGE_NONE][0]['value'], 0, 10)
    : NULL;
  $gender = $person && !empty($person->field_gender[LANGUAGE_NONE][0]['value'])
    ? $person->field_gender[LANGUAGE_NONE][0]['value']
    : NULL;

  // Load all prenatal encounters in this pregnancy with their dates.
  $query = db_select('node', 'n');
  $query->join('field_data_field_individual_participant', 'fp', "fp.entity_id = n.nid AND fp.entity_type = 'node'");
  $query->leftJoin('field_data_field_scheduled_date', 'fsd', "fsd.entity_id = n.nid AND fsd.entity_type = 'node'");
  $query->fields('n', ['nid'])
    ->fields('fsd', ['field_scheduled_date_value'])
    ->condition('n.type', 'prenatal_encounter')
    ->condition('n.status', 1)
    ->condition('fp.field_individual_participant_target_id', $participant_id);
  $encounter_rows = $query->execute()->fetchAll();
  if (empty($encounter_rows)) {
    return $stats;
  }

  $encounter_dates = [];
  foreach ($encounter_rows as $row) {
    $encounter_dates[$row->nid] = !empty($row->field_scheduled_date_value)
      ? substr($row->field_scheduled_date_value, 0, 10)
      : NULL;
  }
  $encounter_ids = array_keys($encounter_dates);

  // Resolve LMP date and (if any) recorded pre-pregnancy weight.
  // The LMP-stored pre-pregnancy weight is rarely populated in practice;
  // we fall back to the earliest prenatal_nutrition weight below.
  $lmp_date = NULL;
  $lmp_pre_pregnancy_weight = NULL;
  $q = db_select('node', 'n');
  $q->join('field_data_field_prenatal_encounter', 'fpe', "fpe.entity_id = n.nid AND fpe.entity_type = 'node'");
  $q->leftJoin('field_data_field_last_menstrual_period', 'flmp', "flmp.entity_id = n.nid AND flmp.entity_type = 'node'");
  $q->leftJoin('field_data_field_weight', 'fw', "fw.entity_id = n.nid AND fw.entity_type = 'node'");
  $q->fields('flmp', ['field_last_menstrual_period_value'])
    ->fields('fw', ['field_weight_value'])
    ->condition('n.type', 'last_menstrual_period')
    ->condition('n.status', 1)
    ->condition('fpe.field_prenatal_encounter_target_id', $encounter_ids, 'IN');
  $lmp_row = $q->execute()->fetchObject();
  if ($lmp_row) {
    $lmp_date = !empty($lmp_row->field_last_menstrual_period_value)
      ? substr($lmp_row->field_last_menstrual_period_value, 0, 10)
      : NULL;
    if ($lmp_row->field_weight_value !== NULL && $lmp_row->field_weight_value !== '') {
      $lmp_pre_pregnancy_weight = (float) $lmp_row->field_weight_value;
    }
  }

  // Mirror Elm resolveGlobalLmpDate: prefer ultrasound EDD over LMP.
  // Take the earliest prenatal_ultrasound (by encounter scheduled_date),
  // subtract 280 days from its EDD to get the implied LMP.
  $q = db_select('node', 'n');
  $q->join('field_data_field_prenatal_encounter', 'fpe', "fpe.entity_id = n.nid AND fpe.entity_type = 'node'");
  $q->leftJoin('field_data_field_expected_date_concluded', 'fedd', "fedd.entity_id = n.nid AND fedd.entity_type = 'node'");
  $q->leftJoin('field_data_field_scheduled_date', 'fsd', "fsd.entity_id = fpe.field_prenatal_encounter_target_id AND fsd.entity_type = 'node'");
  $q->fields('fedd', ['field_expected_date_concluded_value'])
    ->condition('n.type', 'prenatal_ultrasound')
    ->condition('n.status', 1)
    ->condition('fpe.field_prenatal_encounter_target_id', $encounter_ids, 'IN')
    ->isNotNull('fedd.field_expected_date_concluded_value')
    ->orderBy('fsd.field_scheduled_date_value', 'ASC')
    ->range(0, 1);
  $us_row = $q->execute()->fetchObject();
  if ($us_row && !empty($us_row->field_expected_date_concluded_value)) {
    $edd = substr($us_row->field_expected_date_concluded_value, 0, 10);
    $edd_ts = strtotime($edd);
    if ($edd_ts !== FALSE) {
      $lmp_date = date('Y-m-d', $edd_ts - 280 * 86400);
    }
  }

  if (empty($lmp_date)) {
    foreach ($encounter_ids as $eid) {
      $stats['skipped']++;
      $stats['skip_reasons']['no LMP'] = ($stats['skip_reasons']['no LMP'] ?? 0) + 1;
    }
    return $stats;
  }

  // Load all prenatal_nutrition measurements on this pregnancy.
  $q = db_select('node', 'n');
  $q->join('field_data_field_prenatal_encounter', 'fpe', "fpe.entity_id = n.nid AND fpe.entity_type = 'node'");
  $q->leftJoin('field_data_field_weight', 'fw', "fw.entity_id = n.nid AND fw.entity_type = 'node'");
  $q->leftJoin('field_data_field_height', 'fh', "fh.entity_id = n.nid AND fh.entity_type = 'node'");
  $q->leftJoin('field_data_field_muac', 'fm', "fm.entity_id = n.nid AND fm.entity_type = 'node'");
  $q->fields('fpe', ['field_prenatal_encounter_target_id'])
    ->fields('fw', ['field_weight_value'])
    ->fields('fh', ['field_height_value'])
    ->fields('fm', ['field_muac_value'])
    ->condition('n.type', 'prenatal_nutrition')
    ->condition('n.status', 1)
    ->condition('fpe.field_prenatal_encounter_target_id', $encounter_ids, 'IN');
  $nutrition_rows = $q->execute()->fetchAll();
  $nutrition_by_encounter = [];
  foreach ($nutrition_rows as $row) {
    $eid = $row->field_prenatal_encounter_target_id;
    $nutrition_by_encounter[$eid] = [
      'weight' => ($row->field_weight_value !== NULL && $row->field_weight_value !== '') ? (float) $row->field_weight_value : NULL,
      'height' => ($row->field_height_value !== NULL && $row->field_height_value !== '') ? (float) $row->field_height_value : NULL,
      'muac'   => ($row->field_muac_value !== NULL && $row->field_muac_value !== '') ? (float) $row->field_muac_value : NULL,
    ];
  }

  // Build a date-sorted timeline of encounters that have a
  // prenatal_nutrition measurement.
  $timeline = [];
  $empty_data = [
    'weight' => NULL,
    'height' => NULL,
    'muac'   => NULL,
  ];
  foreach ($encounter_dates as $eid => $date) {
    if (empty($date)) {
      continue;
    }
    $data = $nutrition_by_encounter[$eid] ?? $empty_data;
    $timeline[] = ['eid' => $eid, 'date' => $date, 'data' => $data];
  }
  usort($timeline, function ($a, $b) {
    return strcmp($a['date'], $b['date']);
  });

  // Resolve baseline weights.
  $earliest_nutrition_weight = NULL;
  $booking_muac = NULL;
  $baseline_height = NULL;
  foreach ($timeline as $t) {
    if ($earliest_nutrition_weight === NULL && $t['data']['weight'] !== NULL) {
      $earliest_nutrition_weight = $t['data']['weight'];
    }
    if ($booking_muac === NULL && $t['data']['muac'] !== NULL) {
      $booking_muac = $t['data']['muac'];
    }
    if ($baseline_height === NULL && $t['data']['height'] !== NULL) {
      $baseline_height = $t['data']['height'];
    }
  }

  // resolvePrePregnancyWeight: prefer the LMP-stored value, otherwise the
  // earliest prenatal_nutrition weight on the pregnancy.
  $pre_pregnancy_weight = $lmp_pre_pregnancy_weight !== NULL
    ? $lmp_pre_pregnancy_weight
    : $earliest_nutrition_weight;

  for ($i = 0; $i < count($timeline); $i++) {
    $entry = $timeline[$i];
    $eid = $entry['eid'];
    $current_date = $entry['date'];
    $current_weight = $entry['data']['weight'];
    if ($current_weight === NULL) {
      $stats['skipped']++;
      $stats['skip_reasons']['no current weight'] = ($stats['skip_reasons']['no current weight'] ?? 0) + 1;
      continue;
    }

    $encounter_height = $entry['data']['height'] !== NULL ? $entry['data']['height'] : $baseline_height;
    if ($encounter_height === NULL) {
      $stats['skipped']++;
      $stats['skip_reasons']['no height'] = ($stats['skip_reasons']['no height'] ?? 0) + 1;
      continue;
    }

    if ($is_healthy_start) {
      $verdict = classify_gwg_healthy_start(
        $i,
        $timeline,
        $current_date,
        $current_weight,
        $encounter_height,
        $booking_muac,
        $lmp_date,
        $hs_standards,
        $stats['skip_reasons']
      );
    }
    else {
      $verdict = classify_gwg_standard(
        $current_date,
        $current_weight,
        $encounter_height,
        $pre_pregnancy_weight,
        $birth_date,
        $gender,
        $lmp_date,
        $std_standards,
        $stats['skip_reasons']
      );
    }

    if ($verdict === NULL) {
      $stats['skipped']++;
      continue;
    }

    if ($dry_run) {
      $stats['set']++;
      continue;
    }

    $changed = write_gwg_indicator($eid, $verdict);
    if ($changed) {
      $stats['set']++;
    }
    else {
      $stats['unchanged']++;
    }
  }

  return $stats;
}

/**
 * Standard-path GWG classification.
 *
 * @return string|null
 *   'adequate-gwg' / 'inadequate-gwg', or NULL when classification is
 *   not computable (caller increments skip counters via $skip_reasons).
 */
function classify_gwg_standard($current_date, $current_weight, $height_cm, $pre_pregnancy_weight, $birth_date, $gender, $lmp_date, array $std_standards, array &$skip_reasons) {
  if ($pre_pregnancy_weight === NULL) {
    $skip_reasons['STD: no pre-pregnancy weight'] = ($skip_reasons['STD: no pre-pregnancy weight'] ?? 0) + 1;
    return NULL;
  }
  $bmi = compute_bmi($height_cm, $pre_pregnancy_weight);
  if ($bmi === NULL) {
    $skip_reasons['STD: cannot compute BMI'] = ($skip_reasons['STD: cannot compute BMI'] ?? 0) + 1;
    return NULL;
  }
  $classification = classify_pre_pregnancy_standard($bmi, $birth_date, $lmp_date, $gender, $skip_reasons);
  if ($classification === NULL) {
    return NULL;
  }
  $standards = $std_standards[$classification];
  $ega_weeks = floor((strtotime($current_date) - strtotime($lmp_date)) / 86400 / 7);
  if ($ega_weeks <= 12) {
    $expected = $standards['first_trimester'];
  }
  else {
    $expected = $standards['first_trimester'] + ($ega_weeks - 12) * $standards['per_week'];
  }
  if ($expected <= 0) {
    $skip_reasons['STD: zero/negative expected gain'] = ($skip_reasons['STD: zero/negative expected gain'] ?? 0) + 1;
    return NULL;
  }
  $actual = $current_weight - $pre_pregnancy_weight;
  $relation = $actual / $expected;
  return ($relation >= 0.9 && $relation <= 1.25) ? 'adequate-gwg' : 'inadequate-gwg';
}

/**
 * Healthy Start GWG classification.
 *
 * @return string|null
 *   'adequate-gwg' / 'inadequate-gwg', or NULL when classification is
 *   not computable.
 */
function classify_gwg_healthy_start($i, array $timeline, $current_date, $current_weight, $height_cm, $booking_muac, $lmp_date, array $hs_standards, array &$skip_reasons) {
  if ($i === 0) {
    $skip_reasons['HS: first encounter has no prior weight'] = ($skip_reasons['HS: first encounter has no prior weight'] ?? 0) + 1;
    return NULL;
  }
  // Previous encounter that actually has a weight measurement.
  $previous_weight = NULL;
  $previous_weight_date = NULL;
  for ($j = $i - 1; $j >= 0; $j--) {
    if ($timeline[$j]['data']['weight'] !== NULL) {
      $previous_weight = $timeline[$j]['data']['weight'];
      $previous_weight_date = $timeline[$j]['date'];
      break;
    }
  }
  if ($previous_weight === NULL) {
    $skip_reasons['HS: no prior weight in pregnancy'] = ($skip_reasons['HS: no prior weight in pregnancy'] ?? 0) + 1;
    return NULL;
  }

  // Booking weight: earliest prenatal_nutrition weight in the pregnancy.
  $booking_weight = NULL;
  foreach ($timeline as $t) {
    if ($t['data']['weight'] !== NULL) {
      $booking_weight = $t['data']['weight'];
      break;
    }
  }
  $weight_for_baseline_bmi = $booking_weight !== NULL ? $booking_weight : $current_weight;
  $baseline_bmi = compute_bmi($height_cm, $weight_for_baseline_bmi);
  $classification = classify_pre_pregnancy_healthy_start($booking_muac, $baseline_bmi);
  if ($classification === NULL) {
    $skip_reasons['HS: no MUAC and no BMI'] = ($skip_reasons['HS: no MUAC and no BMI'] ?? 0) + 1;
    return NULL;
  }

  $standards = $hs_standards[$classification];
  $thirteen_weeks_ts = strtotime("$lmp_date +13 weeks");
  $current_ts = strtotime($current_date);
  $previous_ts = strtotime($previous_weight_date);
  $total_days = ($current_ts - $previous_ts) / 86400;
  if ($current_ts < $thirteen_weeks_ts) {
    $days_before = $total_days;
    $days_after = 0;
  }
  elseif ($previous_ts >= $thirteen_weeks_ts) {
    $days_before = 0;
    $days_after = $total_days;
  }
  else {
    $days_before = ($thirteen_weeks_ts - $previous_ts) / 86400;
    $days_after = $total_days - $days_before;
  }
  $expected = ($days_before * $standards['before_13w']) + ($days_after * $standards['from_13w']);
  $actual = $current_weight - $previous_weight;
  return ($actual <= $expected) ? 'adequate-gwg' : 'inadequate-gwg';
}

/**
 * Standard-path pre-pregnancy classification.
 *
 * Under 19 years at LMP: BMI-for-age z-score.
 * 19+ years at LMP: adult BMI thresholds.
 *
 * @return string|null
 *   The classification key ('under-weight', 'normal', 'over-weight',
 *   'obesity'), or NULL when classification cannot be computed.
 */
function classify_pre_pregnancy_standard($bmi, $birth_date, $lmp_date, $gender, array &$skip_reasons) {
  if (empty($birth_date) || empty($lmp_date)) {
    $skip_reasons['STD: missing birth date or LMP'] = ($skip_reasons['STD: missing birth date or LMP'] ?? 0) + 1;
    return NULL;
  }
  $age_days = (strtotime($lmp_date) - strtotime($birth_date)) / 86400;
  if ($age_days <= 0) {
    $skip_reasons['STD: non-positive age at LMP'] = ($skip_reasons['STD: non-positive age at LMP'] ?? 0) + 1;
    return NULL;
  }
  $age_years = $age_days / 365.25;
  if ($age_years < 19) {
    if (empty($gender)) {
      $skip_reasons['STD: missing gender for under-19 classification'] = ($skip_reasons['STD: missing gender for under-19 classification'] ?? 0) + 1;
      return NULL;
    }
    $z = hedley_zscore_bmi_for_age($age_days, $gender, $bmi);
    if ($z === NULL || $z === FALSE) {
      $skip_reasons['STD: z-score lookup failed'] = ($skip_reasons['STD: z-score lookup failed'] ?? 0) + 1;
      return NULL;
    }
    return zscore_to_pre_pregnancy_class((float) $z);
  }
  return bmi_to_pre_pregnancy_class($bmi);
}

/**
 * Healthy Start pre-pregnancy classification (two-state).
 *
 * Returns 'under-weight' if (MUAC < 21) or (BMI < 17.5); otherwise 'normal'.
 * Returns NULL when neither MUAC nor BMI is available.
 */
function classify_pre_pregnancy_healthy_start($muac, $bmi) {
  if ($muac === NULL && $bmi === NULL) {
    return NULL;
  }
  if (($muac !== NULL && $muac < 21) || ($bmi !== NULL && $bmi < 17.5)) {
    return 'under-weight';
  }
  return 'normal';
}

/**
 * Mirrors zscoreToPrePregnancyClassification.
 */
function zscore_to_pre_pregnancy_class($z) {
  if ($z < -2) {
    return 'under-weight';
  }
  if ($z <= 1) {
    return 'normal';
  }
  if ($z <= 2) {
    return 'over-weight';
  }
  return 'obesity';
}

/**
 * Mirrors bmiToPrePregnancyClassification.
 */
function bmi_to_pre_pregnancy_class($bmi) {
  if ($bmi < 18.5) {
    return 'under-weight';
  }
  if ($bmi < 25) {
    return 'normal';
  }
  if ($bmi < 30) {
    return 'over-weight';
  }
  return 'obesity';
}

/**
 * Computes BMI from height (cm) and weight (kg).
 */
function compute_bmi($height_cm, $weight_kg) {
  if (empty($height_cm) || empty($weight_kg)) {
    return NULL;
  }
  $h_m = $height_cm / 100;
  if ($h_m == 0) {
    return NULL;
  }
  return $weight_kg / ($h_m * $h_m);
}

/**
 * Writes the GWG verdict onto the encounter's field_prenatal_indicators.
 *
 * Removes any prior 'adequate-gwg'/'inadequate-gwg' values, preserves
 * everything else (e.g. 'past-labs-completed'). Saves only when the set
 * actually changes.
 *
 * @return bool
 *   TRUE when the encounter was saved, FALSE when the verdict was
 *   already present and no save was needed.
 */
function write_gwg_indicator($encounter_id, $verdict) {
  $node = node_load($encounter_id);
  if (!$node) {
    return FALSE;
  }
  $existing = $node->field_prenatal_indicators[LANGUAGE_NONE] ?? [];
  $kept = [];
  $already_correct = FALSE;
  foreach ($existing as $item) {
    $value = $item['value'] ?? NULL;
    if ($value === 'adequate-gwg' || $value === 'inadequate-gwg') {
      if ($value === $verdict) {
        $already_correct = TRUE;
      }
      continue;
    }
    $kept[] = ['value' => $value];
  }
  if ($already_correct && count($kept) === count($existing) - 1) {
    // Verdict already present and no other GWG values to clean up.
    return FALSE;
  }
  $kept[] = ['value' => $verdict];
  $node->field_prenatal_indicators[LANGUAGE_NONE] = $kept;
  node_save($node);
  return TRUE;
}
