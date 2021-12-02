<?php

$queries = [
  "# Patients who have had " =>   "
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
   WHERE
     ip.bundle = 'prenatal_encounter' AND
     fdfpet.field_prenatal_encounter_type_value='nurse'
   GROUP BY
     field_individual_participant_target_id) a
GROUP BY
  val",
  "Of Active Preganancies (within 30 days of EDD AND not completed)" => "SELECT
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
   WHERE
     p.bundle = 'prenatal_encounter' AND
     fdfpet.field_prenatal_encounter_type_value='nurse' AND
     date(edd.field_expected_date_concluded_value) > DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val",
  "Of Completed Pregancies (30 days beyond EDD)" => "SELECT
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
   WHERE
     p.bundle = 'prenatal_encounter' AND
     fdfpet.field_prenatal_encounter_type_value='nurse' AND
     date(edd.field_expected_date_concluded_value) < DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val",
];

$group_limit = 5;
foreach ($queries as $label => $query) {
  // ANC report
  echo "# $label\n";
  $results = db_query($query)->fetchAll(PDO::FETCH_ASSOC);
  $sum_group_limit_or_above = 0;
  foreach ($results as $result) {
    if ($result['val'] < $group_limit) {
      print $result['val'] . " visits\t" . $result['counter'] . "\n";
    }
    else {
      $sum_group_limit_or_above += $result['counter'];
    }
  }
  if (!empty($sum_group_limit_or_above)) {
    print "$group_limit+ visits\t$sum_group_limit_or_above\n";
  }
  echo "\n\n";
}