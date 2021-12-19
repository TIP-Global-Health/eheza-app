<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
 */

require_once __DIR__ . '/report_common.inc';


drush_print("# Acute Illness report  - " . date('D/m/Y'));

$result = db_query("SELECT
  field_acute_illness_diagnosis_value AS type,
  COUNT(*) as counter
FROM
  field_data_field_acute_illness_diagnosis di
LEFT JOIN
  field_data_field_scheduled_date da ON
    di.entity_id = da.entity_id AND
    di.bundle = da.bundle
WHERE
  field_scheduled_date_value > DATE_SUB(CURDATE(), INTERVAL 30 DAY)
GROUP BY
  field_acute_illness_diagnosis_value;");

$diagnosis = [];
$total = 0;
$field = field_info_field('field_acute_illness_diagnosis');
foreach ($result as $item) {
  $diagnosis[] = [
    $field['settings']['allowed_values'][$item->type],
    $item->counter,
  ];
  $total += $item->counter;
}
$diagnosis[] = [
  'Total',
  $total,
];

$text_table = new HedleyAdminTextTable(['Initial Diagnosis in the last 30 days', 'Count']);
drush_print($text_table->render($diagnosis));
