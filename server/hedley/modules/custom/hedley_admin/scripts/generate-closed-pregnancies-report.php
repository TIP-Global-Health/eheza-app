<?php

/**
 * @file
 * Closed pregnancies recurring report.
 */

require_once __DIR__ . '/report_common.inc';

$limit_date = drush_get_option('limit_date', false);
if (!$limit_date) {
    drush_print('Please specify --limit_date option');
    exit;
}

drush_print("# Closed pregnancies report  - " . $limit_date);

$queries = [
  "Outcomes of completed pregnancies (30 days beyond EDD)" => "
SELECT
  field_outcome_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome fo
LEFT JOIN
  node ON fo.entity_id = node.nid
WHERE field_outcome_value NOT IN ('referred-to-hc', 'illness-resolved') AND
FROM_UNIXTIME(node.created) < '$limit_date'
GROUP BY
 field_outcome_value
",
  "Outcome Location" => "
SELECT
  field_outcome_location_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome_location
GROUP BY
  field_outcome_location_value;",
];

foreach ($queries as $label => $query) {
    $table = new HedleyAdminTextTable([$label, 'Counter']);
    $results = db_query($query)->fetchAll(PDO::FETCH_ASSOC);
    $data = [];
    foreach ($results as $result) {
        $data[] = [
        $result['type'],
        $result['counter'],
        ];
    }
    drush_print($table->render($data));
}
