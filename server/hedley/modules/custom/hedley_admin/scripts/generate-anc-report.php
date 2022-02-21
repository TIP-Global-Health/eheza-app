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

$limit_date = drush_get_option('limit_date', false);
if (!$limit_date) {
    drush_print('Please specify --limit_date option');
    exit;
}

drush_print("# ANC report  - " . $limit_date);

$queries = [
  // As the group of all pregnancies.
  "# Patients who have had " => "
SELECT
  val,
  COUNT(*) AS counter
FROM
(SELECT
     COUNT(*) AS val
   FROM
     field_data_field_individual_participant ip
   LEFT JOIN
     field_data_field_prenatal_encounter_type fdfpet ON ip.entity_id = fdfpet.entity_id
   LEFT JOIN
      field_data_field_expected_date_concluded edd ON ip.field_individual_participant_target_id=edd.entity_id
   LEFT JOIN
     node ON ip.entity_id = node.nid
   WHERE
     ip.bundle = 'prenatal_encounter' AND
     (fdfpet.field_prenatal_encounter_type_value='nurse' OR fdfpet.field_prenatal_encounter_type_value is NULL) AND
     edd.field_expected_date_concluded_value is NOT NULL AND
     FROM_UNIXTIME(node.created) < '$limit_date'
   GROUP BY
     field_individual_participant_target_id) a
GROUP BY
  val",
  // As the group of pregnancies which is current - defined as NOT > 30 days
  // past the EDD and AND NOT "closed" (if today is November 1, my pregnancy
  // is current if it is Nov 15 of the same year and the pregnancy
  // has not been closed). The closed-ness of the pregnancy was introduced
  // recently, so this filter is not added there.
  // @todo later, after 1-2 years, it might make sense to add closed filter.
  "Of Active Pregnancies (within 30 days of EDD AND not completed)" => "
SELECT
  val,
  COUNT(*) AS counter
FROM
(SELECT
     COUNT(*) as val
   FROM
     field_data_field_individual_participant p
  LEFT JOIN
     field_data_field_expected_date_concluded edd ON p.field_individual_participant_target_id=edd.entity_id
  LEFT JOIN
     field_data_field_prenatal_encounter_type fdfpet ON p.entity_id = fdfpet.entity_id
  LEFT JOIN
     node ON p.entity_id = node.nid
   WHERE
     p.bundle = 'prenatal_encounter' AND
     (fdfpet.field_prenatal_encounter_type_value='nurse' OR fdfpet.field_prenatal_encounter_type_value is NULL) AND
     date(edd.field_expected_date_concluded_value) > DATE_ADD('$limit_date', INTERVAL 30 DAY) AND
     FROM_UNIXTIME(node.created) < '$limit_date'
  group by
    field_individual_participant_target_id) a
GROUP BY val",
  // As a group of pregnancies that is completed - defined as > 30 past
  // the EDD OR "closed".
  // @todo introduce "closed" when meaningful
  "Of Completed Pregnancies (30 days beyond EDD)" => "
SELECT
  val,
  COUNT(*) AS counter
FROM
(SELECT
     COUNT(*) as val
   FROM
     field_data_field_individual_participant p
   LEFT JOIN
       field_data_field_prenatal_encounter_type fdfpet ON p.entity_id = fdfpet.entity_id
   LEFT JOIN
     field_data_field_expected_date_concluded edd ON p.field_individual_participant_target_id=edd.entity_id
   LEFT JOIN
     node ON p.entity_id = node.nid
   WHERE
     p.bundle = 'prenatal_encounter' AND
     (fdfpet.field_prenatal_encounter_type_value='nurse' OR fdfpet.field_prenatal_encounter_type_value is NULL) AND
     date(edd.field_expected_date_concluded_value) < DATE_ADD('$limit_date', INTERVAL 30 DAY) AND
     FROM_UNIXTIME(node.created) < '$limit_date'
  group by
    field_individual_participant_target_id) a
GROUP BY val",
];

// For the upper end, we group the grouped counters all together, so let's say
// 1 visit => 6
// 5+ visits => 15.
$group_limit = 5;
foreach ($queries as $label => $query) {
    $table = new HedleyAdminTextTable([$label, 'Counter']);
    $data = [];
    $results = db_query($query)->fetchAll(PDO::FETCH_ASSOC);
    $sum_group_limit_or_above = 0;
    foreach ($results as $result) {
        if ($result['val'] < $group_limit) {
            $data[] = [
            $result['val'] . ' visits',
            $result['counter'],
            ];
        }
        else {
            $sum_group_limit_or_above += $result['counter'];
        }
    }
    if (!empty($sum_group_limit_or_above)) {
        $data[] = [
        $group_limit . '+ visits',
        $sum_group_limit_or_above,
        ];
    }
    drush_print($table->render($data));
}
