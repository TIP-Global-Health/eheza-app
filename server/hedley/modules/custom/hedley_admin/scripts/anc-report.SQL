SELECT "Patients who have had" AS '';
SELECT
  concat(val, " visit"),
  COUNT(*)
FROM
  (SELECT
     COUNT(*) AS val
   FROM
     field_data_field_individual_participant
   WHERE
       bundle = 'prenatal_encounter'
   GROUP BY
     field_individual_participant_target_id) a
GROUP BY
  val;

SELECT "Of Active Preganancies (within 30 days of EDD AND not completed)" AS '';
SELECT
  concat(val, " visit"),
  COUNT(*)
FROM
  (SELECT
     COUNT(*) as val
   FROM
     field_data_field_individual_participant p
  LEFT JOIN
     field_data_field_expected_date_concluded edd ON p.field_individual_participant_target_id=edd.entity_id
   WHERE
     p.bundle = 'prenatal_encounter' AND
     date(edd.field_expected_date_concluded_value) < DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val;

SELECT "Of Completed Pregancies (30 days beyond EDD)" AS '';
SELECT
  concat(val, " visit"),
  COUNT(*)
FROM
  (SELECT
     COUNT(*) as val
   FROM
     field_data_field_individual_participant p
  LEFT JOIN
     field_data_field_expected_date_concluded edd ON p.field_individual_participant_target_id=edd.entity_id
   WHERE
     p.bundle = 'prenatal_encounter' AND
     date(edd.field_expected_date_concluded_value) > DATE_ADD(CURDATE(), INTERVAL 30 DAY)
  group by
    field_individual_participant_target_id) a
GROUP BY val;
