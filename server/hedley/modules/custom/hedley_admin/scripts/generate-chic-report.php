<?php

/**
 * @file
 * ANC recurring data report.
 *
 * This report shows the number of visits a woman has had in each pregnancy
 * in three ways
 * Pregnancy visit: ANC for humans, `prenatal` in Drupal.
 */

require_once __DIR__ . '/report_common.inc';

$start_date = drush_get_option('start_date', FALSE);
$end_date = drush_get_option('end_date', FALSE);
if (!$start_date || !$end_date) {
  drush_print('Please specify date options');
  exit;
}

drush_print("# CHIC report - " . $start_date . "-" . $end_date);

$queries = [
  // As the group of all pregnancies.
  "First Visits - Any location" => file_get_contents(__DIR__ . '/anc-first-visit-all-pregnancies-monthly.SQL'),
];

foreach ($queries as $label => $query) {
  $data = [];
  $results = db_query($query, [':start' => $start_date, ':end' => $end_date])->fetchAll(PDO::FETCH_ASSOC);
  foreach ($results as $result) {
    $total_first_visits = $result['first_trimester'] + $result['second_trimester'] + $result['third_trimester'];
    $first_trimester_percentage = ($result['first_trimester'] != 0) ? $result['first_trimester'] / $total_first_visits : '0';
    $second_trimester_percentage = ($result['second_trimester'] != 0) ? $result['second_trimester'] / $total_first_visits : '0';
    $third_trimester_percentage = ($result['third_trimester'] != 0) ? $result['third_trimester'] / $total_first_visits : '0';

    $data = [
      [
        'First Trimester',
        $result['first_trimester'],
        $first_trimester_percentage,
      ],
      [
        'Second Trimester',
        $result['second_trimester'],
        $second_trimester_percentage,
      ],
      [
        'Third Trimester',
        $result['third_trimester'],
        $third_trimester_percentage,
      ],
      [
        'Total',
        $total_first_visits,
        '-',
      ],
    ];
  }

  $text_table = new HedleyAdminTextTable(['Trimester', 'Count', 'Percentage']);
  $text_table->addData($data);

  drush_print('## ' . $label);
  drush_print($text_table->render());
}

drush_print("# Closed pregnancies report - " . $start_date . " - " . $end_date);

$queries = [
  "Outcome Location" => file_get_contents(__DIR__ . '/chic-outcome-location.SQL'),
];

foreach ($queries as $label => $query) {
  $table = new HedleyAdminTextTable([$label, 'Counter']);
  $results = db_query($query, [':start' => $start_date, ':end' => $end_date])->fetchAll(PDO::FETCH_ASSOC);
  $data = [];
  $total_count = 0;
  foreach ($results as $result) {
    $data[] = [
      $result['type'],
      $result['counter'],
    ];
    $total_count = $total_count + $result['counter'];
  }

  $data[] = [
    'Total Deliveries',
    $total_count,
  ];

  drush_print($table->render($data));
}

drush_print("# Newborn Followup report - " . $start_date . " - " . $end_date);

$queries = [
  "Newborn Followup" => file_get_contents(__DIR__ . '/chic-newborn-visit.SQL'),
];

foreach ($queries as $label => $query) {
  $table = new HedleyAdminTextTable([$label, 'Count']);
  $results = db_query($query, [':start' => $start_date, ':end' => $end_date])->fetchAll(PDO::FETCH_ASSOC);
  $data = [];
  foreach ($results as $result) {
    $data[] = [
      'Newborn Visits within 2 days',
      $result['counter'],
    ];

    $data[] = [
      'Total Newborn Visits',
      $result['total'],
    ];
  }

  drush_print($table->render($data));

}

// Get the list of encounters.
$query = file_get_contents(__DIR__ . '/chic-ai-encounters.SQL');
$results = db_query($query, [':start' => $start_date, ':end' => $end_date])->fetchAll(PDO::FETCH_ASSOC);

$nids = [];
foreach ($results as $result) {
  $nids[] = $result['nid'];
}


$symptoms_general[] = [
  'field' => 'field_data_field_body_aches_period',
  'value' => 'field_body_aches_period_value',
  'name' => 'bd',
];
$symptoms_general[] = [
  'field' => 'field_data_field_chills_period',
  'value' => 'field_chills_period_value',
  'name' => 'nbd',
];
$symptoms_general[] = [
  'field' => 'field_data_field_fever_period',
  'value' => 'field_fever_period_value',
  'name' => 'fever',
];
$symptoms_general[] = [
  'field' => 'field_data_field_night_sweats_period',
  'value' => 'field_night_sweats_period_value',
  'name' => 'ns',
];
$symptoms_general[] = [
  'field' => 'field_data_field_headache_period',
  'value' => 'field_headache_period_value',
  'name' => 'headache',
];
$symptoms_general[] = [
  'field' => 'field_data_field_lethargy_period',
  'value' => 'field_lethargy_period_value',
  'name' => 'lethargy',
];
$symptoms_general[] = [
  'field' => 'field_data_field_poor_suck_period',
  'value' => 'field_poor_suck_period_value',
  'name' => 'poor_suck',
];
$symptoms_general[] = [
  'field' => 'field_data_field_unable_to_eat_period',
  'value' => 'field_unable_to_eat_period_value',
  'name' => 'unable_to_eat',
];
$symptoms_general[] = [
  'field' => 'field_data_field_unable_to_drink_period',
  'value' => 'field_unable_to_drink_period_value',
  'name' => 'unable_to_drink',
];

$symptoms_general[] = [
  'field' => 'field_data_field_increased_thirst_period',
  'value' => 'field_increased_thirst_period_value',
  'name' => 'increased_thirst',
];
$symptoms_general[] = [
  'field' => 'field_data_field_dry_mouth_period',
  'value' => 'field_dry_mouth_period_value',
  'name' => 'dry_mouth',
];
$symptoms_general[] = [
  'field' => 'field_data_field_severe_weakness_period',
  'value' => 'field_severe_weakness_period_value',
  'name' => 'severe_weakness',
];
$total_count = 0;
$success_count = 0;

foreach ($nids as $nid) {

  $query = "SELECT entity_id FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_general' AND field_acute_illness_encounter_target_id = $nid LIMIT 1";

  $symptom_id = db_query($query)->fetchfield();

  if ($symptom_id) {
    foreach ($symptoms_general as $symptom) {
      $query = 'SELECT ' . $symptom['value'] . ' as value FROM ' . $symptom['field'] . ' WHERE entity_id = ' . $symptom_id;
      $data = db_query($query)->fetchfield();
      if ($data > 1) {
        $total_count = $total_count + 1;
        break;
      }
    }
    $total_count = $total_count + 1;
    $success_count = $success_count + 1;
  }
}

$table = new HedleyAdminTextTable(['Encounters', 'Count']);


$data = [];
$data[] = [
  'AI Visits within 2 days',
  $success_count,
];

$data[] = [
  'Total Encounters',
  $total_count,
];

drush_print($table->render($data));

$nodes = [];

foreach ($nids as $nid) {
  $general = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_general' AND field_acute_illness_encounter_target_id = $nid")->fetchfield();
  $respiratory = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_respiratory' AND field_acute_illness_encounter_target_id = $nid")->fetchfield();
  $gi = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_gi' AND field_acute_illness_encounter_target_id = $nid")->fetchfield();

  $nodes[$nid] = [
    'general' => $general,
    'respiratory' => $respiratory,
    'gi' => $gi,
  ];

}

$table = new HedleyAdminTextTable([
  'Encounter ID',
  'General',
  'Respiratory',
  'GI',
]);

$data = [];
foreach ($nodes as $nid => $value) {
  $data[] = [
    $nid,
    $value['general'],
    $value['respiratory'],
    $value['gi'],
  ];
}
