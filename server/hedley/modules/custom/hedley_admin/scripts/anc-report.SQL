SELECT "Patients who have had" AS '';
SELECT
  concat(val, " visit") as '',
  COUNT(*) as ''
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
  val;

SELECT "Of Active Preganancies (within 30 days of EDD AND not completed)" AS '';
SELECT
  concat(val, " visit") AS '',
  COUNT(*) AS ''
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
GROUP BY val;

SELECT "Of Completed Pregancies (30 days beyond EDD)" AS '';
SELECT
  concat(val, " visit") AS '',
  COUNT(*) AS ''
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
GROUP BY val;
