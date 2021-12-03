<?php

// This report shows the number of visits a woman has had in each pregnancy
// in three ways.
// Pregnancy visit: ANC for humans, `prenatal` in Drupal.

$queries = [
  // As the group of all pregnancies.
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
  // As the group of pregnancies which is current - defined as NOT > 30 days
  // past the EDD and AND NOT "closed" (e.g. if today is November 1, my pregnancy
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
   WHERE
     p.bundle = 'prenatal_encounter' AND
     fdfpet.field_prenatal_encounter_type_value='nurse' AND
     date(edd.field_expected_date_concluded_value) > DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val",
  // As a group of pregnancies that is completed - defined as > 30 past the EDD OR "closed"
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
   WHERE
     p.bundle = 'prenatal_encounter' AND
     fdfpet.field_prenatal_encounter_type_value='nurse' AND
     date(edd.field_expected_date_concluded_value) < DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val",
];

// For the upper end, we group the grouped counters all together, so let's say
// 1 visit => 6
// 5+ visits => 15.
$group_limit = 5;
foreach ($queries as $label => $query) {
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