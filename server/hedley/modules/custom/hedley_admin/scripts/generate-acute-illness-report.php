<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
 */

require_once __DIR__ . '/report_common.inc';


drush_print("# Acute Illness report - " . date('D/m/Y'));

$result = db_query("SELECT
  field_acute_illness_diagnosis_value AS type,
  COUNT(*) as counter
FROM
  field_data_field_acute_illness_diagnosis di
LEFT JOIN
  field_data_field_scheduled_date da ON
    di.entity_id = da.entity_id AND
    di.bundle = da.bundle
LEFT JOIN field_data_field_individual_participant ip ON di.entity_id = ip.entity_id
LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
WHERE
  field_scheduled_date_value >= '2022-08-01'
  AND field_scheduled_date_value < '2022-09-01'
  AND field_health_center_target_id = '1246786'
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
