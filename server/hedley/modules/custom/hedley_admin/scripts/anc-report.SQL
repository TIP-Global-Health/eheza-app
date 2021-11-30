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