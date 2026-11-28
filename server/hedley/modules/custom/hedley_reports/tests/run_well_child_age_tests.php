<?php

/**
 * @file
 * Standalone tests of the age a Well Child completion row is counted at.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * variable_get() reads the values set by set_variable(), and node_load()
 * returns the nodes set by store_node().
 *
 * The app counts a month of a child's age as reached on the month-end day
 * the birth date falls on: a child born 31 March is 6 months old on
 * 30 September, and a child born 29 February 2024 is 36 months old on
 * 28 February 2027. The Well Child rows of the Completion report must count
 * the same way, so that they expect the activities the app offered. The
 * Nutrition and Acute Illness rows keep their own count.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Keeps the Drupal variables and nodes the stubs read.
 */
function test_storage($bucket, $key = NULL, $value = NULL) {
  static $storage = [
    'nodes' => [],
    'variables' => [
      'hedley_admin_feature_ncda_enabled' => TRUE,
      'hedley_general_site_name' => 'rwanda',
    ],
  ];
  if ($value !== NULL) {
    $storage[$bucket][$key] = $value;
  }
  return $key === NULL ? $storage[$bucket] : ($storage[$bucket][$key] ?? NULL);
}

/**
 * Drupal variable_get() stub.
 */
function variable_get($name, $default = NULL) {
  $variables = test_storage('variables');
  return array_key_exists($name, $variables) ? $variables[$name] : $default;
}

/**
 * Sets a Drupal variable for the cases that follow.
 */
function set_variable($name, $value) {
  test_storage('variables', $name, $value);
}

/**
 * Sets the site, 'rwanda' or 'burundi', for the cases that follow.
 */
function set_site($site) {
  set_variable('hedley_general_site_name', $site);
}

/**
 * Drupal node_load() stub.
 */
function node_load($nid) {
  $node = test_storage('nodes', $nid);
  return $node === NULL ? FALSE : $node;
}

/**
 * Stores a node for node_load().
 */
function store_node($nid, $node) {
  test_storage('nodes', $nid, $node);
}

/**
 * Drupal watchdog() stub.
 */
function watchdog($type, $message) {
}

/**
 * Returns the age in months the Well Child builder counts on a visit day.
 */
function well_child_age($birth_date, $visit_date) {
  return hedley_reports_age_in_months_reached(new DateTime($birth_date), new DateTime($visit_date));
}

/**
 * Builds a previous Well Child encounter, with or without an ECD assessment.
 *
 * @param string $date
 *   The day of the encounter, as Y-m-d.
 * @param array|null $ecd_signs
 *   The ECD signs recorded, or NULL when ECD was not assessed.
 */
function previous_encounter($date, $ecd_signs = NULL) {
  $ecd = NULL;
  if ($ecd_signs !== NULL) {
    $ecd = new stdClass();
    $ecd->field_date_measured[LANGUAGE_NONE][0]['value'] = $date;
    $ecd->field_ecd_signs[LANGUAGE_NONE] = array_map(function ($sign) {
      return ['value' => $sign];
    }, $ecd_signs);
  }

  return [
    'encounter_type' => 'pediatric-care',
    'measurements_by_type' => ['well_child_ecd' => $ecd],
    'start_date' => $date,
    'start_date_obj' => new DateTime($date),
  ];
}

/**
 * Returns a vaccination history with no doses of any vaccine.
 */
function no_vaccinations() {
  $vaccines = [
    'bcg',
    'dtp',
    'dtp_sa',
    'hpv',
    'ipv',
    'mr',
    'opv',
    'pcv13',
    'rotarix',
  ];
  return array_fill_keys($vaccines, []);
}

/**
 * Builds the encounter data the Well Child builder makes for a visit.
 *
 * The ages come from the same calls the builder makes, so a case reads the
 * ages the report counts for that visit: the Well Child age, and the age
 * the NCDA check reads, which keeps the count of the individual encounters.
 */
function well_child_encounter($birth_date, $visit_date, array $previous_encounters = [], array $vaccinations = NULL, $encounter_type = 'pediatric-care') {
  $vaccinations = $vaccinations ?? no_vaccinations();
  return [
    'age_in_months' => well_child_age($birth_date, $visit_date),
    'age_in_months_for_ncda' => individual_encounter_age($birth_date, $visit_date),
    'encounter_type' => $encounter_type,
    'measurements_by_type' => [],
    'previous_encounters_data' => $previous_encounters,
    'start_date' => $visit_date,
    'start_date_obj' => new DateTime($visit_date),
    'vaccination_history' => $vaccinations,
    'vaccination_progress' => $vaccinations,
  ];
}

/**
 * Returns the activities a Well Child row expects, as the builder adds them.
 */
function expected_activities($birth_date, $visit_date, array $previous_encounters = [], array $vaccinations = NULL, $encounter_type = 'pediatric-care') {
  $birth_date_obj = new DateTime($birth_date);
  $encounter_data = well_child_encounter($birth_date, $visit_date, $previous_encounters, $vaccinations, $encounter_type);
  $expected = hedley_reports_well_child_generate_expected_initial_activities($encounter_data);
  hedley_reports_well_child_add_ecd_activity($encounter_data, $birth_date_obj, $expected);
  hedley_reports_well_child_add_immunisation_activities($encounter_data, $birth_date_obj, 'female', $expected);
  hedley_reports_well_child_add_medication_activities($encounter_data, $expected);
  return $expected;
}

/**
 * Tells, for each activity, whether a Well Child row expects it.
 *
 * Example: expects(['well_child_muac'], ...) gives ['well_child_muac' => TRUE]
 * when MUAC is expected.
 */
function expects(array $activities, $birth_date, $visit_date, array $previous_encounters = [], array $vaccinations = NULL, $encounter_type = 'pediatric-care') {
  $expected = expected_activities($birth_date, $visit_date, $previous_encounters, $vaccinations, $encounter_type);
  $result = [];
  foreach ($activities as $activity) {
    $result[$activity] = in_array($activity, $expected, TRUE);
  }
  return $result;
}

/**
 * Returns the ECD signs a Well Child row expects at a visit.
 */
function expected_ecd_signs($birth_date, $visit_date, array $previous_encounters = []) {
  $encounter_data = well_child_encounter($birth_date, $visit_date, $previous_encounters);
  return hedley_reports_well_child_generate_expected_ecd_signs($encounter_data, new DateTime($birth_date));
}

/**
 * Tells, for each sign, whether it is among the expected ECD signs.
 */
function expects_ecd_signs(array $signs, $birth_date, $visit_date, array $previous_encounters = []) {
  $expected = expected_ecd_signs($birth_date, $visit_date, $previous_encounters);
  $result = [];
  foreach ($signs as $sign) {
    $result[$sign] = in_array($sign, $expected, TRUE);
  }
  return $result;
}

/**
 * Returns a vaccination history with three DTP doses, the last on a date.
 */
function three_dtp_doses($third_dose_date) {
  $vaccinations = no_vaccinations();
  $vaccinations['dtp'] = [
    'dose-1' => '2025-05-12',
    'dose-2' => '2025-06-09',
    'dose-3' => $third_dose_date,
  ];
  return $vaccinations;
}

/**
 * Tells whether the DTP booster is expected at a Well Child visit.
 */
function dtp_booster_expected($birth_date, $visit_date, array $vaccinations) {
  $encounter_data = well_child_encounter($birth_date, $visit_date, [], $vaccinations);
  $expected = hedley_reports_well_child_get_expected_immunisation_activities($encounter_data, new DateTime($birth_date), 'female', 'history');
  return in_array('well_child_dtp_sa_immunisation', $expected, TRUE);
}

/**
 * Returns the age a Nutrition or Acute Illness row counts at an encounter.
 */
function individual_encounter_age($birth_date, $encounter_date) {
  $person = new stdClass();
  $person->field_birth_date[LANGUAGE_NONE][0]['value'] = "$birth_date 00:00:00";
  store_node(2, $person);
  $participant = new stdClass();
  $participant->field_person[LANGUAGE_NONE][0]['target_id'] = 2;
  store_node(1, $participant);
  $encounter = new stdClass();
  $encounter->nid = 3;
  $encounter->field_individual_participant[LANGUAGE_NONE][0]['target_id'] = 1;
  return hedley_reports_resolve_age_in_months_for_individual_encounter($encounter, new DateTime($encounter_date));
}

/**
 * Builds a previous Well Child encounter with Medication answers.
 *
 * @param string $date
 *   The day of the encounter, as Y-m-d.
 * @param array $notes
 *   The administration note per medication activity, like
 *   ['well_child_vitamin_a' => 'administered-today'] when it was given, or a
 *   reason it was not, like 'lack-of-stock'.
 */
function medication_encounter($date, array $notes) {
  $measurements = [];
  foreach ($notes as $activity => $note) {
    $measurement = new stdClass();
    $measurement->field_administration_note[LANGUAGE_NONE][0]['value'] = $note;
    $measurements[$activity] = $measurement;
  }

  return [
    'encounter_type' => 'pediatric-care',
    'measurements_by_type' => $measurements,
    'start_date' => $date,
    'start_date_obj' => new DateTime($date),
  ];
}

/**
 * Tells whether a Well Child visit expects a medication activity.
 */
function medication_expected($activity, $birth_date, $visit_date, array $previous_encounters) {
  $encounter_data = well_child_encounter($birth_date, $visit_date, $previous_encounters);
  $expected = [];
  hedley_reports_well_child_add_medication_activities($encounter_data, $expected);
  return in_array($activity, $expected, TRUE);
}

/**
 * Checks that a medication given on a day is expected again from a due day.
 *
 * The visit the day before the due day does not expect it, and the visit on
 * the due day does.
 */
function assert_medication_due_on($activity, $birth_date, $given_date, $due_date, $case, &$passed, &$failed) {
  $previous = [medication_encounter($given_date, [$activity => 'administered-today'])];
  $day_before = (new DateTime($due_date))->modify('-1 day')->format('Y-m-d');
  $result = [
    medication_expected($activity, $birth_date, $day_before, $previous),
    medication_expected($activity, $birth_date, $due_date, $previous),
  ];
  assert_same($result, [FALSE, TRUE], $case, $passed, $failed);
}

set_site('rwanda');

// Criterion 1: a child born 31 March, seen at a Well Child visit on
// 30 September, counts as 6 months old: MUAC and Vitamin A are expected.
assert_same(well_child_age('2025-03-31', '2025-09-30'), 6, 'A child born 31 March, seen on 30 September, counts as 6 months old', $passed, $failed);
assert_same(expects(['well_child_muac', 'well_child_vitamin_a'], '2025-03-31', '2025-09-30'), [
  'well_child_muac' => TRUE,
  'well_child_vitamin_a' => TRUE,
], 'A child born 31 March, seen at a Well Child visit on 30 September: MUAC and Vitamin A are expected (Rwanda)', $passed, $failed);
set_site('burundi');
$burundi_six_months = [
  'well_child_muac',
  'well_child_vitamin_a',
  'well_child_albendazole',
];
assert_same(expects($burundi_six_months, '2025-03-31', '2025-09-30'), [
  'well_child_muac' => TRUE,
  'well_child_vitamin_a' => TRUE,
  'well_child_albendazole' => TRUE,
], 'A child born 31 March, seen at a Well Child visit on 30 September: MUAC, Vitamin A and Albendazole are expected (Burundi)', $passed, $failed);
set_site('rwanda');

// Boundaries of criterion 1. The day before the month-end day the child is
// still 5 months old, and the day after the month-end day 6 months old.
assert_same(well_child_age('2025-03-31', '2025-09-29'), 5, 'A child born 31 March, seen on 29 September, is still 5 months old', $passed, $failed);
assert_same(expects(['well_child_muac', 'well_child_vitamin_a'], '2025-03-31', '2025-09-29'), [
  'well_child_muac' => FALSE,
  'well_child_vitamin_a' => FALSE,
], 'A child born 31 March, seen on 29 September: MUAC and Vitamin A are not expected', $passed, $failed);
assert_same(well_child_age('2025-03-31', '2025-10-01'), 6, 'A child born 31 March, seen on 1 October, is 6 months old', $passed, $failed);
// A newborn exam never expects MUAC, whatever the age counts.
assert_same(expects(['well_child_muac'], '2025-03-31', '2025-09-30', [], NULL, 'newborn-exam'), [
  'well_child_muac' => FALSE,
], 'A newborn exam on the month-end day does not expect MUAC', $passed, $failed);
// The other month-end shapes: a 30-day month after a 31st birth day, a
// short February in common and leap years, and a year boundary.
assert_same(well_child_age('2025-03-31', '2025-04-30'), 1, 'A child born 31 March is 1 month old on 30 April', $passed, $failed);
assert_same(well_child_age('2025-03-31', '2026-04-30'), 13, 'A child born 31 March 2025 is 13 months old on 30 April 2026', $passed, $failed);
assert_same(well_child_age('2025-01-31', '2025-02-28'), 1, 'A child born 31 January 2025 is 1 month old on 28 February 2025', $passed, $failed);
assert_same(well_child_age('2025-01-30', '2025-02-28'), 1, 'A child born 30 January 2025 is 1 month old on 28 February 2025', $passed, $failed);
assert_same(well_child_age('2025-01-29', '2025-02-28'), 1, 'A child born 29 January 2025 is 1 month old on 28 February 2025', $passed, $failed);
assert_same(well_child_age('2025-01-31', '2025-02-27'), 0, 'A child born 31 January 2025 is not yet 1 month old on 27 February 2025', $passed, $failed);
assert_same(well_child_age('2024-01-31', '2024-02-29'), 1, 'A child born 31 January 2024 is 1 month old on 29 February 2024', $passed, $failed);
assert_same(well_child_age('2024-01-31', '2024-02-28'), 0, 'A child born 31 January 2024 is not yet 1 month old on 28 February 2024, a leap year', $passed, $failed);
assert_same(well_child_age('2024-12-31', '2025-11-30'), 11, 'A child born 31 December 2024 is 11 months old on 30 November 2025', $passed, $failed);
// A birth on the 30th meets 30 September on the same day of the month.
assert_same(well_child_age('2025-03-30', '2025-09-30'), 6, 'A child born 30 March is 6 months old on 30 September', $passed, $failed);

// Criterion 2: a child born 29 February 2024, seen on 28 February 2027,
// counts as 36 months old: head circumference is not expected.
assert_same(well_child_age('2024-02-29', '2027-02-28'), 36, 'A child born 29 February 2024, seen on 28 February 2027, counts as 36 months old', $passed, $failed);
assert_same(expects(['well_child_head_circumference'], '2024-02-29', '2027-02-28'), [
  'well_child_head_circumference' => FALSE,
], 'A child born 29 February 2024, seen on 28 February 2027: head circumference is not expected', $passed, $failed);

// Boundaries of criterion 2. The day before, the child is 35 months old and
// head circumference is expected. On 28 February of a leap year the month
// has not ended, so the month is not reached yet.
assert_same(expects(['well_child_head_circumference'], '2024-02-29', '2027-02-27'), [
  'well_child_head_circumference' => TRUE,
], 'A child born 29 February 2024, seen on 27 February 2027: head circumference is expected', $passed, $failed);
assert_same(well_child_age('2024-02-29', '2025-02-28'), 12, 'A child born 29 February 2024 is 12 months old on 28 February 2025', $passed, $failed);
assert_same(well_child_age('2024-02-29', '2028-02-28'), 47, 'A child born 29 February 2024 is still 47 months old on 28 February 2028, a leap year', $passed, $failed);
assert_same(well_child_age('2024-02-29', '2028-02-29'), 48, 'A child born 29 February 2024 is 48 months old on 29 February 2028', $passed, $failed);
// Mebendazole starts at 12 months on Rwanda, so the first birthday on
// 28 February 2025 expects it.
assert_same(expects(['well_child_mebendezole'], '2024-02-29', '2025-02-28'), [
  'well_child_mebendezole' => TRUE,
], 'A child born 29 February 2024, seen on 28 February 2025: Mebendazole is expected (Rwanda)', $passed, $failed);
// NCDA keeps today's count, as the app asks it then. NCDA is expected under
// 24 months: a child born 29 February 2024 is still 23 months old for NCDA
// on 28 February 2026, though 24 months for the rest of the row, and turns
// 24 for NCDA on 1 March 2026.
assert_same(expects(['well_child_ncda'], '2024-02-29', '2026-02-28'), [
  'well_child_ncda' => TRUE,
], 'NCDA keeps today\'s count: a child born 29 February 2024, seen on 28 February 2026, still has NCDA expected', $passed, $failed);
// The rest of that row counts 24 months, so the ECD signs from 2 years are
// expected at the same visit that still expects NCDA.
assert_same(expects_ecd_signs(['use-short-phrases'], '2024-02-29', '2026-02-28'), [
  'use-short-phrases' => TRUE,
], 'A child born 29 February 2024, seen on 28 February 2026: the ECD signs from 2 years are expected', $passed, $failed);
assert_same(expects(['well_child_ncda'], '2024-02-29', '2026-02-27'), [
  'well_child_ncda' => TRUE,
], 'A child born 29 February 2024, seen on 27 February 2026: NCDA is expected', $passed, $failed);
assert_same(expects(['well_child_ncda'], '2024-02-29', '2026-03-01'), [
  'well_child_ncda' => FALSE,
], 'A child born 29 February 2024, seen on 1 March 2026: NCDA is not expected', $passed, $failed);
assert_same(expects(['well_child_ncda'], '2024-02-29', '2026-02-28', [], NULL, 'pediatric-care-chw'), [
  'well_child_ncda' => FALSE,
], 'A CHW visit on 28 February 2026 for a child born 29 February 2024: NCDA is not expected', $passed, $failed);

// Criterion 3: the same child seen on 28 February 2030 counts as 72 months:
// on Rwanda, Vitamin A and Mebendazole are not expected, and Albendazole is.
assert_same(well_child_age('2024-02-29', '2030-02-28'), 72, 'The same child seen on 28 February 2030 counts as 72 months', $passed, $failed);
$rwanda_medications = [
  'well_child_vitamin_a',
  'well_child_mebendezole',
  'well_child_albendazole',
];
assert_same(expects($rwanda_medications, '2024-02-29', '2030-02-28'), [
  'well_child_vitamin_a' => FALSE,
  'well_child_mebendezole' => FALSE,
  'well_child_albendazole' => TRUE,
], 'On Rwanda, at 72 months on 28 February 2030: Vitamin A and Mebendazole are not expected, and Albendazole is', $passed, $failed);

// Boundaries of criterion 3: the day before on Rwanda, and the other site.
assert_same(expects($rwanda_medications, '2024-02-29', '2030-02-27'), [
  'well_child_vitamin_a' => TRUE,
  'well_child_mebendezole' => TRUE,
  'well_child_albendazole' => FALSE,
], 'On Rwanda, at 71 months on 27 February 2030: Vitamin A and Mebendazole are expected, and Albendazole is not', $passed, $failed);
set_site('burundi');
assert_same(expects(['well_child_vitamin_a', 'well_child_albendazole'], '2024-02-29', '2030-02-28'), [
  'well_child_vitamin_a' => FALSE,
  'well_child_albendazole' => TRUE,
], 'On Burundi, at 72 months on 28 February 2030: Vitamin A is not expected, and Albendazole is', $passed, $failed);
assert_same(expects(['well_child_vitamin_a', 'well_child_albendazole'], '2024-02-29', '2030-02-27'), [
  'well_child_vitamin_a' => TRUE,
  'well_child_albendazole' => TRUE,
], 'On Burundi, at 71 months on 27 February 2030: Vitamin A and Albendazole are expected', $passed, $failed);

// Criterion 4: on Burundi, a child born 31 March 2025 with three DTP doses,
// the last at least 28 days earlier, seen on 30 September 2026, is 18 months
// old: the DTP booster is expected.
assert_same(well_child_age('2025-03-31', '2026-09-30'), 18, 'A child born 31 March 2025, seen on 30 September 2026, is 18 months old', $passed, $failed);
assert_same(dtp_booster_expected('2025-03-31', '2026-09-30', three_dtp_doses('2025-07-07')), TRUE, 'On Burundi, born 31 March 2025 with three DTP doses, the last at least 28 days earlier, seen on 30 September 2026: the DTP booster is expected', $passed, $failed);
assert_same(dtp_booster_expected('2025-03-31', '2026-09-30', three_dtp_doses('2026-09-02')), TRUE, 'On Burundi, born 31 March 2025, third DTP dose exactly 28 days before 30 September 2026: the DTP booster is expected', $passed, $failed);

// Boundaries of criterion 4: a day younger, a third dose 27 days earlier,
// only two doses, and the other site.
assert_same(dtp_booster_expected('2025-03-31', '2026-09-29', three_dtp_doses('2025-07-07')), FALSE, 'On Burundi, born 31 March 2025, seen on 29 September 2026 at 17 months: the DTP booster is not expected', $passed, $failed);
assert_same(dtp_booster_expected('2025-03-31', '2026-09-30', three_dtp_doses('2026-09-03')), FALSE, 'On Burundi, born 31 March 2025, third DTP dose 27 days before 30 September 2026: the DTP booster is not expected', $passed, $failed);
$two_dtp_doses = three_dtp_doses('2025-07-07');
unset($two_dtp_doses['dtp']['dose-3']);
assert_same(dtp_booster_expected('2025-03-31', '2026-09-30', $two_dtp_doses), FALSE, 'On Burundi, born 31 March 2025 with two DTP doses, seen on 30 September 2026: the DTP booster is not expected', $passed, $failed);
set_site('rwanda');
assert_same(dtp_booster_expected('2025-03-31', '2026-09-30', three_dtp_doses('2025-07-07')), FALSE, 'On Rwanda, born 31 March 2025 with three DTP doses, seen on 30 September 2026: the DTP booster is not expected', $passed, $failed);

// Criterion 5, the age on the visit day. A child born 31 March 2025 with no
// earlier ECD assessment is 6 months old on 30 September 2025, so the signs
// for 6 to 12 months are expected along with the earlier ones.
$signs_up_to_12_months = [
  'follow-mothers-eyes',
  'smile',
  'sit-without-support',
  'use-consonant-sounds',
];
assert_same(expects_ecd_signs($signs_up_to_12_months, '2025-03-31', '2025-09-30'), [
  'follow-mothers-eyes' => TRUE,
  'smile' => TRUE,
  'sit-without-support' => TRUE,
  'use-consonant-sounds' => TRUE,
], 'The ECD signs expected on 30 September for a child born 31 March use the same count for the age on the visit day', $passed, $failed);
// A child born 29 February 2024, 36 months old on 28 February 2027, is
// expected to show the signs from 3 years.
assert_same(expects_ecd_signs(['dress-themselves', 'use-short-phrases'], '2024-02-29', '2027-02-28'), [
  'dress-themselves' => TRUE,
  'use-short-phrases' => TRUE,
], 'The ECD signs expected on 28 February 2027 for a child born 29 February 2024 include the signs from 3 years', $passed, $failed);
// The day before the month-end day the 6 to 12 month signs are not expected.
assert_same(expects_ecd_signs(['follow-mothers-eyes', 'sit-without-support'], '2025-03-31', '2025-09-29'), [
  'follow-mothers-eyes' => TRUE,
  'sit-without-support' => FALSE,
], 'The ECD signs expected on 29 September for a child born 31 March leave out the 6 to 12 month signs', $passed, $failed);

// Criterion 5, the age at the last ECD assessment. A child born 31 March
// 2025 was assessed on 30 September 2025, at 6 months, so the signs from 5
// and 13 weeks are not asked again on 15 November 2025.
$assessed_at_six_months = [previous_encounter('2025-09-30', ['none'])];
assert_same(expects_ecd_signs($signs_up_to_12_months, '2025-03-31', '2025-11-15', $assessed_at_six_months), [
  'follow-mothers-eyes' => FALSE,
  'smile' => FALSE,
  'sit-without-support' => TRUE,
  'use-consonant-sounds' => TRUE,
], 'The ECD signs expected after an assessment on 30 September, for a child born 31 March, use the same count for the age at the last assessment', $passed, $failed);
// Assessed on 30 April 2026, at 13 months, the signs for 6 to 12 months are
// not asked again on 15 June 2026, at 14 months. Nothing is left to expect
// before 15 months.
assert_same(expected_ecd_signs('2025-03-31', '2026-06-15', [previous_encounter('2026-04-30', ['none'])]), [], 'After an assessment on 30 April 2026 at 13 months, for a child born 31 March 2025, no ECD signs are expected at 14 months', $passed, $failed);
// Boundaries: assessed the day before the month-end day, at 5 months, the
// signs from 5 and 13 weeks are still expected. The latest assessment is the
// one that counts.
assert_same(expects_ecd_signs(['follow-mothers-eyes', 'sit-without-support'], '2025-03-31', '2025-11-15', [previous_encounter('2025-09-29', ['none'])]), [
  'follow-mothers-eyes' => TRUE,
  'sit-without-support' => TRUE,
], 'After an assessment on 29 September at 5 months, for a child born 31 March, the signs from 5 weeks are still expected', $passed, $failed);
assert_same(expects_ecd_signs(['follow-mothers-eyes'], '2025-03-31', '2025-11-15', [
  previous_encounter('2025-07-15', ['none']),
  previous_encounter('2025-08-15'),
  previous_encounter('2025-09-30', ['none']),
  previous_encounter('2025-10-15'),
]), [
  'follow-mothers-eyes' => FALSE,
], 'The latest ECD assessment, on 30 September at 6 months, decides the signs expected, not an earlier one or a visit without ECD', $passed, $failed);

// Criterion 5, through the ECD activity. The signs from 5 and 13 weeks were
// all recorded at 3 months. On 30 September, at 6 months, the signs for 6 to
// 12 months are due, so the ECD activity is expected.
$early_signs = array_merge(hedley_reports_well_child_ecd_signs_from_5_weeks(), hedley_reports_well_child_ecd_signs_from_13_weeks());
assert_same(expects(['well_child_ecd'], '2025-03-31', '2025-09-30', [previous_encounter('2025-07-15', $early_signs)]), [
  'well_child_ecd' => TRUE,
], 'ECD is expected on 30 September for a child born 31 March who has shown every sign up to 13 weeks', $passed, $failed);
assert_same(expects(['well_child_ecd'], '2025-03-31', '2025-09-29', [previous_encounter('2025-07-15', $early_signs)]), [
  'well_child_ecd' => FALSE,
], 'ECD is not expected on 29 September for a child born 31 March who has shown every sign up to 13 weeks', $passed, $failed);

// Criterion 6: a visit on any other day is counted as today. A child born
// 15 March and seen on 14 September is 5 months old, and MUAC is not
// expected. On 15 September the child is 6 months old and MUAC is expected.
assert_same(well_child_age('2025-03-15', '2025-09-14'), 5, 'A child born 15 March and seen on 14 September is 5 months old', $passed, $failed);
assert_same(expects(['well_child_muac'], '2025-03-15', '2025-09-14'), [
  'well_child_muac' => FALSE,
], 'A child born 15 March and seen on 14 September: MUAC is not expected', $passed, $failed);
assert_same(well_child_age('2025-03-15', '2025-09-15'), 6, 'A child born 15 March and seen on 15 September is 6 months old', $passed, $failed);
assert_same(expects(['well_child_muac'], '2025-03-15', '2025-09-15'), [
  'well_child_muac' => TRUE,
], 'A child born 15 March and seen on 15 September: MUAC is expected', $passed, $failed);
assert_same(well_child_age('2025-01-31', '2025-03-30'), 1, 'A child born 31 January and seen on 30 March is 1 month old', $passed, $failed);
assert_same(well_child_age('2025-01-31', '2025-03-31'), 2, 'A child born 31 January and seen on 31 March is 2 months old', $passed, $failed);
assert_same(well_child_age('2024-02-29', '2027-03-01'), 36, 'A child born 29 February 2024 and seen on 1 March 2027 is 36 months old', $passed, $failed);

// Criterion 7: the Nutrition and Acute Illness rows of the report are
// unchanged. Their age at an encounter keeps its own count, so a month-end
// day is still counted as a month younger there.
assert_same(individual_encounter_age('2025-03-31', '2025-09-30'), 5, 'The Nutrition and Acute Illness rows are unchanged: born 31 March, seen on 30 September, 5 months', $passed, $failed);
assert_same(individual_encounter_age('2024-02-29', '2027-02-28'), 35, 'The Nutrition and Acute Illness rows are unchanged: born 29 February 2024, seen on 28 February 2027, 35 months', $passed, $failed);
assert_same(individual_encounter_age('2025-03-15', '2025-09-14'), 5, 'The Nutrition and Acute Illness rows are unchanged: born 15 March, seen on 14 September, 5 months', $passed, $failed);
assert_same(individual_encounter_age('2025-03-15', '2025-09-15'), 6, 'The Nutrition and Acute Illness rows are unchanged: born 15 March, seen on 15 September, 6 months', $passed, $failed);

// Medications given on the 29th to the 31st. The app adds 6 months and stops
// on the last day of a shorter month: given on 31 August 2025, the next dose
// is offered from 28 February 2026. The report must expect it from the same
// day, or a skipped dose in the days between is not counted as missing.
// Children are born mid-month, far from any age limit, so the age-based list
// of medications is the same on every visit day of a case.
set_site('rwanda');
$born_2023 = '2023-01-15';
$born_2022 = '2022-01-15';
$born_2018 = '2018-01-15';

// Criterion: Vitamin A given on 2025-08-31 is expected again on 2026-02-28,
// and not on 2026-02-27.
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-31', '2026-02-28', 'Vitamin A given at a Well Child visit on 2025-08-31: a visit on 2026-02-28 expects it again. A visit on 2026-02-27 does not.', $passed, $failed);

// Criterion: Albendazole or Mebendazole, the same. Mebendazole is Rwanda's
// from 1 to 6 years; Albendazole is Rwanda's from 6 years and Burundi's from
// 6 months.
assert_medication_due_on('well_child_mebendezole', $born_2023, '2025-08-31', '2026-02-28', 'Mebendazole given on 2025-08-31: a visit on 2026-02-28 expects it again, a visit on 2026-02-27 does not (Rwanda)', $passed, $failed);
assert_medication_due_on('well_child_albendazole', $born_2018, '2025-08-31', '2026-02-28', 'Albendazole given on 2025-08-31: a visit on 2026-02-28 expects it again, a visit on 2026-02-27 does not (Rwanda)', $passed, $failed);
set_site('burundi');
assert_medication_due_on('well_child_albendazole', $born_2023, '2025-08-31', '2026-02-28', 'Albendazole given on 2025-08-31: a visit on 2026-02-28 expects it again, a visit on 2026-02-27 does not (Burundi)', $passed, $failed);
// Vitamin A on the other site follows the same rule.
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-31', '2026-02-28', 'Boundary: Vitamin A given on 2025-08-31: a visit on 2026-02-28 expects it again, a visit on 2026-02-27 does not (Burundi)', $passed, $failed);
set_site('rwanda');

// Criterion: Vitamin A given on 2023-08-31 is expected again on 2024-02-29,
// the last day of February in a leap year, and not on 2024-02-28.
assert_medication_due_on('well_child_vitamin_a', $born_2022, '2023-08-31', '2024-02-29', 'Vitamin A given on 2023-08-31: a visit on 2024-02-29 expects it again', $passed, $failed);

// Boundary: between the app's day and the day the report used to wait for,
// the medication stays expected.
$vitamin_a_on_31_august = [medication_encounter('2025-08-31', ['well_child_vitamin_a' => 'administered-today'])];
assert_same(medication_expected('well_child_vitamin_a', $born_2023, '2026-03-02', $vitamin_a_on_31_august), TRUE, 'Boundary: Vitamin A given on 2025-08-31 is still expected on 2026-03-02', $passed, $failed);
assert_same(medication_expected('well_child_vitamin_a', $born_2023, '2026-03-03', $vitamin_a_on_31_august), TRUE, 'guard: Vitamin A given on 2025-08-31 is still expected on 2026-03-03', $passed, $failed);

// Boundary: the 29th and 30th also run past the end of February 2026.
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-30', '2026-02-28', 'Boundary: Vitamin A given on 2025-08-30 is expected again from 2026-02-28', $passed, $failed);
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-29', '2026-02-28', 'Boundary: Vitamin A given on 2025-08-29 is expected again from 2026-02-28', $passed, $failed);
// In a leap year 29 February exists, so the 29th keeps its day.
assert_medication_due_on('well_child_vitamin_a', $born_2022, '2023-08-29', '2024-02-29', 'Boundary: Vitamin A given on 2023-08-29 is expected again from 2024-02-29, not 2024-02-28', $passed, $failed);
// A 30-day month that is not February, and a year boundary.
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-03-31', '2025-09-30', 'Boundary: Vitamin A given on 2025-03-31 is expected again from 2025-09-30', $passed, $failed);
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-12-31', '2026-06-30', 'Boundary: Vitamin A given on 2025-12-31 is expected again from 2026-06-30', $passed, $failed);

// Boundary: the latest dose given decides, not an earlier one, and a later
// visit where it was not given does not count as a dose.
$vitamin_a_history = [
  medication_encounter('2025-02-15', ['well_child_vitamin_a' => 'administered-today']),
  medication_encounter('2025-08-31', ['well_child_vitamin_a' => 'administered-today']),
  medication_encounter('2025-10-15', ['well_child_vitamin_a' => 'lack-of-stock']),
];
assert_same([
  medication_expected('well_child_vitamin_a', $born_2023, '2026-02-27', $vitamin_a_history),
  medication_expected('well_child_vitamin_a', $born_2023, '2026-02-28', $vitamin_a_history),
], [FALSE, TRUE], 'Boundary: the latest Vitamin A given, on 2025-08-31, makes it due from 2026-02-28, whatever came before or after', $passed, $failed);
// Boundary: one medication given on the 31st does not move another one.
assert_same(medication_expected('well_child_mebendezole', $born_2023, '2026-02-27', $vitamin_a_on_31_august), TRUE, 'guard: Vitamin A given on 2025-08-31 leaves Mebendazole, never given, expected', $passed, $failed);

// Guards: a first encounter, and a medication never given, expect it.
assert_same(medication_expected('well_child_vitamin_a', $born_2023, '2026-02-27', []), TRUE, 'guard: Vitamin A at a first encounter is expected', $passed, $failed);
assert_same(medication_expected('well_child_vitamin_a', $born_2023, '2026-02-27', [
  medication_encounter('2025-08-31', ['well_child_vitamin_a' => 'patient-declined']),
]), TRUE, 'guard: Vitamin A declined on 2025-08-31 is expected at the next visit', $passed, $failed);

// Criterion: a dose given on the 1st to the 28th is due on the same day of
// the month as before.
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-15', '2026-02-15', 'A dose given on the 1st to the 28th is due on the same day of the month as before: Vitamin A given on 2025-08-15 is due on 2026-02-15', $passed, $failed);
assert_medication_due_on('well_child_mebendezole', $born_2023, '2025-08-28', '2026-02-28', 'guard: Mebendazole given on 2025-08-28 is due on 2026-02-28', $passed, $failed);
assert_medication_due_on('well_child_vitamin_a', $born_2023, '2025-08-01', '2026-02-01', 'guard: Vitamin A given on 2025-08-01 is due on 2026-02-01', $passed, $failed);

report_results('Well Child age', $passed, $failed);
