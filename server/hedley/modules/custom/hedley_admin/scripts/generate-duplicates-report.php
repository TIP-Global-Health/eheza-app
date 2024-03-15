<?php

/**
 * @file
 * Generates 'Duplicates' report by Health Center.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-duplicates-report.php.
 */

require_once __DIR__ . '/report_common.inc';

$health_center = drush_get_option('health_center', FALSE);
if (!$health_center) {
  drush_print('Please specify --health_center');
  exit;
}

/**
 * Gets the list of duplicates for a given village/health center.
 *
 * @param int $health_center_id
 *   The node ID of a health center.
 * @param string $village
 *   The name of a village.
 *
 * @return array
 *   The node ID of the health center.
 */
function get_duplicates($health_center_id, $village = NULL) {
  return db_query("SELECT node.nid, fn.field_first_name_value, sn.field_second_name_value, bd.field_birth_date_value, v.field_village_value
    FROM field_data_field_birth_date bd
    LEFT JOIN node ON bd.entity_id = node.nid
    LEFT JOIN field_data_field_first_name fn ON node.nid = fn.entity_id
    LEFT JOIN field_data_field_second_name sn ON node.nid = sn.entity_id
    LEFT JOIN field_data_field_village v ON node.nid = v.entity_id
    LEFT JOIN field_data_field_health_center hc ON node.nid = hc.entity_id
    WHERE bd.field_birth_date_value
      IN (SELECT dbd.field_birth_date_value
          FROM field_data_field_birth_date dbd
          LEFT JOIN node dnode ON dbd.entity_id = dnode.nid
          LEFT JOIN field_data_field_village dv ON dnode.nid = dv.entity_id
          LEFT JOIN field_data_field_health_center dhc ON dnode.nid = dhc.entity_id
          WHERE UPPER(dv.field_village_value) = UPPER('$village')
          AND dhc.field_health_center_target_id= $health_center_id
          GROUP BY dbd.field_birth_date_value
          HAVING COUNT(dbd.field_birth_date_value) > 1)
      AND bd.field_birth_date_value NOT LIKE '%01-01%'
      AND UPPER(v.field_village_value) = UPPER('$village')
      AND hc.field_health_center_target_id= $health_center_id
      ORDER BY `bd`.`field_birth_date_value` DESC")->fetchAll();
}

/**
 * Gets the list of villages in a health center.
 *
 * @param int $health_center_id
 *   The name of a healthcenter.
 *
 * @return array
 *   The list of villages.
 */
function get_villages($health_center_id) {
  return db_query("SELECT DISTINCT field_village_value
    FROM field_data_field_village v
    LEFT JOIN field_data_field_health_center hc ON hc.entity_id = v.entity_id
    WHERE hc.field_health_center_target_id = $health_center_id
    ORDER BY v.field_village_value ASC")->fetchCol();
}

// Get the health center ID from the health center name and find its villages.
$health_center_id = get_health_center_id($health_center);
$villages = get_villages($health_center_id);

// Iterate over each village.
$data = [];
foreach ($villages as $village) {
  // Get the duplicates for this village.
  $results = get_duplicates($health_center_id, $village);

  // Add teh results to teh $data array.
  foreach ($results as $result) {
    $data[] = [
      $result->nid,
      $result->field_first_name_value,
      $result->field_second_name_value,
      date("Y-m-d", strtotime($result->field_birth_date_value)),
      strtoupper($result->field_village_value),
    ];
  }
}

drush_print("# Duplicates report - " . $health_center . "(" . $health_center_id . ")");

$table = new HedleyAdminTextTable([
  'Node ID',
  'First Name',
  'Second Name',
  'Birth Date',
  'Village',
]);

drush_print($table->render($data));
