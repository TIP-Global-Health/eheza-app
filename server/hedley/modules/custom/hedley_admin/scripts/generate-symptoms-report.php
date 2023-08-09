<?php

/**
 * @file
 * AI Symptoms Report.
 *
 * This report shows the of symptom nodes connected to 
 * each enconter node by type.
 */

require_once __DIR__ . '/report_common.inc';


drush_print("# Symptoms report");

// Get the list of encounters.
$query = file_get_contents(__DIR__ . '/chic-ai-encounters.SQL');
$results = db_query($query, [':start' => '2023-01-01', ':end' => '2023-01-31'])->fetchAll(PDO::FETCH_ASSOC);

$nodes = [];

foreach ($results as $result) {
  $general = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_general' AND field_acute_illness_encounter_target_id = $result['nid']")->fetchfield();
  $respiratory = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_respiratory' AND field_acute_illness_encounter_target_id = $result['nid']")->fetchfield();
  $gi = db_query("SELECT COUNT(*) AS counter FROM field_data_field_acute_illness_encounter WHERE bundle = 'symptoms_gi' AND field_acute_illness_encounter_target_id = $result['nid']")->fetchfield();

  $nodes[$result['nid']] = [
    'general' => $general, 
    'respiratory' => $respiratory,
    'gi' => $gi,
  ];

}

$table = new HedleyAdminTextTable(['Encounter ID', 'General', 'Respiratory', 'GI']);

$data =[];
foreach ($nodes as $nid => $value) {
  $data[] = [
    $nid,
    $value['general'],
    $value['respiratory'],
    $value['gi'],
  ];
}

drush_print($table->render($data));

