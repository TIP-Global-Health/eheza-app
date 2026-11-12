<?php

/**
 * @file
 * Standalone tests of the Prenatal completion rules.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Builds a danger signs measurement with the given signs.
 */
function danger_signs(array $signs) {
  $measurement = new stdClass();
  $measurement->field_danger_signs[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $signs);
  return $measurement;
}

/**
 * Returns the activities expected of a CHW encounter with given measurements.
 *
 * @param string $type
 *   The encounter type.
 * @param array $activities
 *   The activities recorded at the encounter, keyed by name.
 * @param array $expected
 *   The activities expected before Health Education and Next Steps are added.
 * @param array $previous_nurse_encounters
 *   The nurse encounters before this one.
 */
function chw_expected($type, array $activities, array $expected = ['danger_signs'], array $previous_nurse_encounters = []) {
  $encounter_data = [
    'encounter_type' => $type,
    'measurements_by_type' => $activities,
    'previous_nurse_encounters_data' => $previous_nurse_encounters,
    'start_date_obj' => new DateTime('2026-01-01'),
  ];
  hedley_reports_prenatal_add_health_education_activity_for_chw($encounter_data, $expected);
  hedley_reports_prenatal_add_next_steps_activities($encounter_data, $expected);
  return $expected;
}

/**
 * Tells which of the given activities are expected.
 */
function expected_among(array $expected, array $activities) {
  return array_values(array_intersect($activities, $expected));
}

$next_steps = [
  'appointment_confirmation',
  'prenatal_send_to_hc',
  'prenatal_follow_up',
];
$recorded = new stdClass();
$no_signs = danger_signs(['none']);
$with_signs = danger_signs(['vaginal-bleeding']);

assert_same(
  expected_among(chw_expected('chw-1', ['danger_signs' => $no_signs]), $next_steps),
  [],
  'chw-1, no danger signs, no Health Education: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ]), $next_steps),
  ['appointment_confirmation', 'prenatal_follow_up'],
  'chw-1, no danger signs, Health Education done: Appointment Confirmation and Follow Up',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', ['danger_signs' => $with_signs]), $next_steps),
  ['prenatal_send_to_hc', 'prenatal_follow_up'],
  'chw-1, danger signs, no Health Education: Send to HC and Follow Up',
  $passed,
  $failed
);

assert_same(
  // The rules read a missing danger signs record as an undefined index.
  expected_among(@chw_expected('chw-1', []), $next_steps),
  [],
  'chw-1, danger signs not recorded: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'last_menstrual_period', 'pregnancy_testing']), $next_steps),
  [],
  'chw-1, first visit without LMP and pregnancy testing: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'last_menstrual_period'], [['id' => 1]]), $next_steps),
  ['appointment_confirmation', 'prenatal_follow_up'],
  'chw-1 after a nurse visit: a missing LMP does not hold Next Steps back',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-2', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'birth_plan']), $next_steps),
  [],
  'chw-2, no danger signs, no Birth Plan: no Next Steps',
  $passed,
  $failed
);

foreach (['chw-2', 'chw-3'] as $type) {
  $has_birth_plan = $type == 'chw-2';
  $base_expected = $has_birth_plan ? ['danger_signs', 'birth_plan'] : ['danger_signs'];
  $base_activities = $has_birth_plan ? ['birth_plan' => $recorded] : [];

  assert_same(
    expected_among(chw_expected($type, $base_activities + ['danger_signs' => $no_signs], $base_expected), $next_steps),
    [],
    "$type, no danger signs, no Health Education: no Next Steps",
    $passed,
    $failed
  );

  assert_same(
    expected_among(chw_expected($type, $base_activities + [
      'danger_signs' => $no_signs,
      'prenatal_health_education' => $recorded,
    ], $base_expected), $next_steps),
    ['appointment_confirmation', 'prenatal_follow_up'],
    "$type, no danger signs, Health Education done: Appointment Confirmation and Follow Up",
    $passed,
    $failed
  );

  assert_same(
    expected_among(chw_expected($type, ['danger_signs' => $with_signs]), $next_steps),
    ['prenatal_send_to_hc', 'prenatal_follow_up'],
    "$type, danger signs: Send to HC and Follow Up",
    $passed,
    $failed
  );
}

/**
 * Stands in for Drupal's variable_get().
 *
 * The switch to one Medication activity per supplement happened long before
 * every visit in these tests.
 */
function variable_get($name, $default = NULL) {
  return $name == 'hedley_prenatal_change_medications' ? '2020-01-01' : $default;
}

/**
 * Builds a Medication activity record of one supplement with the given note.
 *
 * The note is 'administered-today' when the supplement was handed over, or a
 * reason it was not, such as 'lack-of-stock' or 'patient-declined'.
 */
function supplement($note) {
  $measurement = new stdClass();
  $measurement->field_administration_note[LANGUAGE_NONE] = [['value' => $note]];
  return $measurement;
}

/**
 * Builds Treatment Review's prenatal medication answer.
 *
 * The signs are what the woman says was received: 'mebendezole',
 * 'deworming-pill', 'iron-and-folic-acid-supplement', or 'none' for "No" to
 * all.
 */
function treatment_review(array $signs) {
  $measurement = new stdClass();
  $measurement->field_medication[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $signs);
  $measurement->field_hiv_treatment[LANGUAGE_NONE] = [];
  $measurement->field_hypertension_treatment[LANGUAGE_NONE] = [];
  $measurement->field_malaria_treatment[LANGUAGE_NONE] = [];
  $measurement->field_anemia_treatment[LANGUAGE_NONE] = [];
  $measurement->field_syphilis_treatment[LANGUAGE_NONE] = [];
  return $measurement;
}

/**
 * Builds Next Steps' medication distribution.
 *
 * The prescribed medication lists what was handed out: 'mebendezole', or
 * 'none' with a non administration reason when Mebendazole was out of stock.
 */
function medication_distribution(array $prescribed, array $non_administration = []) {
  $measurement = new stdClass();
  $measurement->field_prescribed_medication[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $prescribed);
  $measurement->field_non_administration_reason[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $non_administration);
  return $measurement;
}

/**
 * Fills the measurement slots a nurse visit's rules read, leaving them empty.
 */
function nurse_measurements(array $measurements) {
  $slots = [
    'medication',
    'prenatal_calcium',
    'prenatal_fefol',
    'prenatal_folate',
    'prenatal_hiv_test',
    'prenatal_iron',
    'prenatal_mebendazole',
    'prenatal_medication_distribution',
    'prenatal_mms',
  ];
  return $measurements + array_fill_keys($slots, NULL);
}

/**
 * Builds the data of an earlier nurse visit, held weeks before 2026-06-01.
 */
function earlier_nurse_visit($weeks_ago, array $measurements) {
  $start_date_obj = new DateTime('2026-06-01');
  $start_date_obj->modify("-$weeks_ago weeks");
  return [
    'encounter_type' => 'nurse',
    'start_date_obj' => $start_date_obj,
    'diagnoses' => [],
    'measurements_by_type' => nurse_measurements($measurements),
  ];
}

/**
 * Builds the data of a nurse follow-up visit on 2026-06-01.
 *
 * @param int $ega_weeks
 *   The pregnancy's EGA in weeks at this visit, set through the LMP.
 * @param array $earlier_visits
 *   The earlier nurse visits, oldest first, built by earlier_nurse_visit().
 * @param array $measurements
 *   The measurements of this visit, keyed by type.
 */
function nurse_follow_up_visit($ega_weeks, array $earlier_visits, array $measurements = []) {
  $start_date_obj = new DateTime('2026-06-01');
  $lmp_date_obj = clone $start_date_obj;
  $lmp_date_obj->modify("-$ega_weeks weeks");
  return [
    'encounter_type' => 'nurse',
    'start_date_obj' => $start_date_obj,
    'lmp_date' => $lmp_date_obj->format('Y-m-d'),
    'diagnoses' => [],
    'past_diagnoses' => [],
    'measurements_by_type' => nurse_measurements($measurements),
    'previous_nurse_encounters_data' => $earlier_visits,
  ];
}

/**
 * Tells whether the visit is expected to hold the Medication activity's task.
 */
function medication_task_expected(array $encounter_data, $task) {
  $expected = [];
  hedley_reports_prenatal_add_medication_activities($encounter_data, $expected);
  return in_array($task, $expected);
}

/**
 * Tells whether the visit is expected to hold Next Steps' distribution.
 */
function distribution_expected(array $encounter_data) {
  return hedley_reports_prenatal_medication_distribution_expected_per_medication_set($encounter_data);
}

// SUPPLEMENTS AT NURSE FOLLOW-UP VISITS
//
// The Medication activity hands out Mebendazole from 24 weeks. Next Steps'
// medication distribution can hand it out too, after Treatment Review's
// "received Mebendazole?" question; an earlier variation of that question
// asked about a deworming pill. The Completion report must see every route:
// it must not count a visit as missing the Medication activity's Mebendazole,
// or Next Steps' Mebendazole, when it was already handed out or the woman said
// it was received.
//
// Every scenario is a nurse follow-up visit on 2026-06-01, after the switch to
// one Medication activity per supplement. Earlier visits are dated a number of
// weeks before it.
$mebendazole_given = ['prenatal_mebendazole' => supplement('administered-today')];
$mebendazole_declined = ['prenatal_mebendazole' => supplement('patient-declined')];
$mebendazole_out_of_stock = ['prenatal_mebendazole' => supplement('lack-of-stock')];
$handed_out_from_next_steps = ['prenatal_medication_distribution' => medication_distribution(['mebendezole'])];
$out_of_stock_in_next_steps = [
  'prenatal_medication_distribution' => medication_distribution(['none'], ['mebendezole-lack-of-stock']),
];
$said_received_mebendazole = ['medication' => treatment_review(['mebendezole'])];
$said_received_deworming_pill = ['medication' => treatment_review(['deworming-pill'])];
$said_received_nothing = ['medication' => treatment_review(['none'])];
$said_iron_folic_acid = ['medication' => treatment_review(['iron-and-folic-acid-supplement'])];

// The Completion report does not count a visit as missing the Medication
// activity's Mebendazole when it was given from Next Steps, or the woman said
// it was received, at an earlier visit or the current one.
$criterion = 'Medication Mebendazole not missing when given from Next Steps or said received';

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $handed_out_from_next_steps)]), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: handed out from Next Steps at an earlier visit",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [
    earlier_nurse_visit(8, $handed_out_from_next_steps),
    earlier_nurse_visit(4, []),
  ]), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: handed out from Next Steps two visits ago, nothing at the last visit",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $handed_out_from_next_steps), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: handed out from Next Steps at the current visit",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_received_mebendazole)]), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: she said at an earlier visit that she received Mebendazole",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [
    earlier_nurse_visit(8, $said_received_mebendazole),
    earlier_nurse_visit(4, $said_received_nothing),
  ]), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: she said two visits ago that she received Mebendazole, and No at the last visit",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $said_received_mebendazole), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: she says at the current visit that she received Mebendazole",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_received_deworming_pill)]), 'prenatal_mebendazole'),
  FALSE,
  "$criterion: she said at an earlier visit that she received a deworming pill",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $mebendazole_given)]), 'prenatal_mebendazole'),
  FALSE,
  "guard: $criterion: given through Medication at an earlier visit",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])]), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: nothing at the earlier visit, 30 weeks: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(24, [earlier_nurse_visit(4, [])]), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: nothing at the earlier visit, exactly 24 weeks: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(23, [earlier_nurse_visit(4, $said_received_nothing)]), 'prenatal_mebendazole'),
  FALSE,
  "guard: $criterion: 23 weeks: not expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $out_of_stock_in_next_steps)]), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: out of stock in Next Steps at an earlier visit: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $out_of_stock_in_next_steps), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: out of stock in Next Steps at the current visit: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_received_nothing)], $said_received_nothing), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: she answered No at the earlier visit and at the current one: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_iron_folic_acid)]), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: she said at an earlier visit that she takes iron / folic acid only: expected",
  $passed,
  $failed
);

assert_same(
  medication_task_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $mebendazole_declined)]), 'prenatal_mebendazole'),
  TRUE,
  "guard: $criterion: declined in Medication at an earlier visit: expected",
  $passed,
  $failed
);

// Saying in Treatment Review that the woman takes iron / folic acid does not
// stop Fefol, iron or folate being offered, so the report keeps expecting
// them.
foreach (['prenatal_fefol', 'prenatal_iron', 'prenatal_folate'] as $task) {
  assert_same(
    medication_task_expected(nurse_follow_up_visit(12, [earlier_nurse_visit(4, $said_iron_folic_acid)], $said_iron_folic_acid), $task),
    TRUE,
    "guard: iron / folic acid said at an earlier visit and the current one: $task expected",
    $passed,
    $failed
  );
}

// The Completion report does not expect Mebendazole in the Next Steps
// medication distribution once it was given through Medication at an earlier
// visit or the current one, or the woman said at an earlier visit that it was
// received. No diagnosis is set, so Mebendazole is the only medication set
// that can make the distribution expected.
$criterion = 'Next Steps Mebendazole not expected once given through Medication or said received earlier';

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $mebendazole_given)], $said_received_nothing)),
  FALSE,
  "$criterion: given through Medication at an earlier visit",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [
    earlier_nurse_visit(8, $mebendazole_given),
    earlier_nurse_visit(4, $said_received_nothing),
  ], $said_received_nothing)),
  FALSE,
  "$criterion: given through Medication two visits ago, No at the last visit",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $mebendazole_given + $said_received_nothing)),
  FALSE,
  "$criterion: given through Medication at the current visit, she answered No",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $mebendazole_given)),
  FALSE,
  "$criterion: given through Medication at the current visit, Treatment Review not answered",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_received_mebendazole)], $said_received_nothing)),
  FALSE,
  "$criterion: she said at an earlier visit that she received Mebendazole",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [
    earlier_nurse_visit(8, $said_received_mebendazole),
    earlier_nurse_visit(4, $said_received_nothing),
  ], $said_received_nothing)),
  FALSE,
  "$criterion: she said two visits ago that she received Mebendazole, and No at the last visit",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_received_deworming_pill)], $said_received_nothing)),
  FALSE,
  "guard: $criterion: she said at an earlier visit that she received a deworming pill",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $handed_out_from_next_steps)], $said_received_nothing)),
  FALSE,
  "guard: $criterion: handed out from Next Steps at an earlier visit",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $said_received_mebendazole)),
  FALSE,
  "guard: $criterion: she says at the current visit that she received Mebendazole",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $said_received_nothing)),
  TRUE,
  "guard: $criterion: nothing earlier, she answers No: expected",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $mebendazole_declined)], $said_received_nothing)),
  TRUE,
  "guard: $criterion: declined in Medication at an earlier visit, she answers No: expected",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, [])], $mebendazole_out_of_stock + $said_received_nothing)),
  TRUE,
  "guard: $criterion: out of stock in Medication at the current visit, she answers No: expected",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(30, [earlier_nurse_visit(4, $said_iron_folic_acid)], $said_received_nothing)),
  TRUE,
  "guard: $criterion: she said at an earlier visit that she takes iron / folic acid only: expected",
  $passed,
  $failed
);

assert_same(
  distribution_expected(nurse_follow_up_visit(23, [earlier_nurse_visit(4, [])], $said_received_nothing)),
  FALSE,
  "guard: $criterion: 23 weeks: not expected",
  $passed,
  $failed
);

report_results('Prenatal completion', $passed, $failed);
