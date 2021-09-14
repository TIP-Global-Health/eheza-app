SELECT '----------------------' AS '';

# Generate individual encounter report.
SELECT 'Prenatal report:' AS '';
SELECT
  CONCAT('Completed encounters: ', COUNT(DISTINCT field_prenatal_encounter_target_id)) AS ''
FROM
  field_data_field_prenatal_encounter;
SELECT
  CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS ''
FROM
  field_data_field_prenatal_encounter e
    LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id;

SELECT 'Nutrition report:' AS '';
SELECT
  CONCAT('Completed encounters: ', COUNT(DISTINCT field_nutrition_encounter_target_id)) AS ''
FROM
  field_data_field_nutrition_encounter;
SELECT
  CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS ''
FROM
  field_data_field_nutrition_encounter e
    LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id;

SELECT 'Acute illness report:' AS '';
SELECT
  CONCAT('Completed encounters: ', COUNT(DISTINCT field_acute_illness_encounter_target_id)) AS ''
FROM
  field_data_field_acute_illness_encounter;
SELECT
  CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS ''
FROM
  field_data_field_acute_illness_encounter e
    LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id;

# Classify by age and gender.
DROP TABLE IF EXISTS person_classified;
CREATE TABLE person_classified
(
  `entity_id` int(10) UNSIGNED NOT NULL COMMENT 'The entity id this data is attached to',
  age         varchar(10),
  gender      varchar(10)
);
ALTER TABLE person_classified
  ADD PRIMARY KEY entity_id (entity_id);

INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt1m'             AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
  field_birth_date_value + INTERVAL 1 MONTH > CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt2y'             AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
    field_birth_date_value + INTERVAL 2 YEAR > CURDATE()
AND field_birth_date_value + INTERVAL 1 MONTH <= CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt5y'             AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
    field_birth_date_value + INTERVAL 5 YEAR > CURDATE()
AND field_birth_date_value + INTERVAL 2 YEAR <= CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt10y'            AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
    field_birth_date_value + INTERVAL 10 YEAR > CURDATE()
AND field_birth_date_value + INTERVAL 5 YEAR <= CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt20y'            AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
    field_birth_date_value + INTERVAL 20 YEAR > CURDATE()
AND field_birth_date_value + INTERVAL 10 YEAR <= CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'lt50y'            AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
    field_birth_date_value + INTERVAL 50 YEAR > CURDATE()
AND field_birth_date_value + INTERVAL 20 YEAR <= CURDATE();
INSERT
INTO
  person_classified
SELECT
  b.entity_id,
  'mt50y'            AS age,
  field_gender_value AS gender
FROM
  field_data_field_birth_date b
    LEFT JOIN field_data_field_gender g ON b.entity_id = g.entity_id
WHERE
  field_birth_date_value + INTERVAL 50 YEAR < CURDATE();
# Exclude deleted person.
DELETE
FROM
  person_classified
WHERE
    entity_id IN (SELECT
                    entity_id
                  FROM
                    field_data_field_deleted
                  WHERE
                    field_deleted_value = 1);
# Exclude unpublished person.
DELETE
FROM
  person_classified
WHERE
    entity_id IN (SELECT
                   nid
                  FROM
                   node
                  WHERE
                      status= 0 AND
                      type='person');

SELECT '----------------------' AS '';
SELECT 'Groups report:' AS '';
SELECT
  CONCAT(UPPER(field_group_type_value), ' encounters:  ', COUNT(*)) AS ''
FROM
  (
    SELECT
      field_group_type_value,
      p.field_person_target_id,
      sess_rel.field_session_target_id
    FROM
      field_data_field_session sess_rel
        LEFT JOIN field_data_field_clinic c ON sess_rel.field_session_target_id = c.entity_id
        LEFT JOIN field_data_field_group_type gt ON field_clinic_target_id = gt.entity_id
        LEFT JOIN field_data_field_person p ON p.entity_id = sess_rel.entity_id
        LEFT JOIN person_classified class ON p.field_person_target_id = class.entity_id
    WHERE
        sess_rel.bundle IN ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors',
                            'core_physical_exam',
                            'danger_signs', 'family_planning', 'follow_up', 'group_health_education',
                            'group_send_to_hc', 'height',
                            'lactation', 'last_menstrual_period', 'medical_history', 'medication',
                            'mother_fbf', 'muac', 'nutrition',
                            'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo',
                            'nutrition_weight',
                            'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo',
                            'pregnancy_testing',
                            'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition',
                            'prenatal_photo',
                            'resource', 'social_history', 'vitals', 'weight', 'acute_findings',
                            'acute_illness_danger_signs',
                            'acute_illness_follow_up', 'acute_illness_muac', 'acute_illness_nutrition',
                            'acute_illness_vitals',
                            'call_114', 'exposure', 'hc_contact', 'health_education', 'isolation',
                            'malaria_testing',
                            'medication_distribution', 'send_to_hc', 'symptoms_general', 'symptoms_gi',
                            'symptoms_respiratory',
                            'travel_history', 'treatment_history', 'treatment_ongoing',
                            'participant_consent', 'nutrition_caring',
                            'nutrition_contributing_factors', 'nutrition_feeding', 'nutrition_follow_up',
                            'nutrition_food_security',
                            'nutrition_health_education', 'nutrition_hygiene', 'nutrition_send_to_hc',
                            'appointment_confirmation',
                            'prenatal_follow_up', 'prenatal_send_to_hc', 'counseling_session')
    AND field_group_type_value IS NOT NULL
    AND class.entity_id IS NOT NULL
    GROUP BY
      field_group_type_value, field_person_target_id, field_session_target_id
  ) b
GROUP BY
  field_group_type_value;

SELECT '----------------------' AS '';

SELECT 'Registered patients report: ' AS '';
SELECT '* 0 - 1 month:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt1m'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* 1 month - 2 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt2y'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* 2 years - 5 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt5y'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* 5 years - 10 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt10y'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* 10 years - 20 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt20y'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* 20 years - 50 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'lt50y'
GROUP BY
  gender
ORDER BY
  gender DESC;
SELECT '* Older than 50 years:' AS '';
SELECT
  CONCAT(gender, ' - ', COUNT(*)) AS ''
FROM
  person_classified
WHERE
  age = 'mt50y'
GROUP BY
  gender
ORDER BY
  gender DESC;

SELECT '----------------------' AS '';

DROP PROCEDURE IF EXISTS impacted_report;
DELIMITER //
CREATE PROCEDURE impacted_report(IN age_group VARCHAR(6))
BEGIN
  SELECT
    CONCAT(gender, ' - ', COUNT(*)) AS ''
  FROM
    person_classified
  WHERE
      age = age_group
  AND entity_id IN (
    SELECT
      p.field_person_target_id
    FROM
      node ms
        LEFT JOIN field_data_field_person p ON p.entity_id = ms.nid
        LEFT JOIN field_data_field_session sess ON sess.entity_id = ms.nid
        LEFT JOIN field_data_field_clinic clinic ON sess.field_session_target_id=clinic.entity_id
        LEFT JOIN field_data_field_nutrition_encounter nutr ON nutr.entity_id = ms.nid
        LEFT JOIN field_data_field_prenatal_encounter pren ON pren.entity_id = ms.nid
        LEFT JOIN field_data_field_acute_illness_encounter acute ON acute.entity_id = ms.nid
    WHERE
        ms.type IN ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors',
                    'core_physical_exam',
                    'danger_signs', 'family_planning', 'follow_up', 'group_health_education',
                    'group_send_to_hc', 'height',
                    'lactation', 'last_menstrual_period', 'medical_history', 'medication',
                    'mother_fbf', 'muac', 'nutrition',
                    'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo',
                    'nutrition_weight',
                    'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo',
                    'pregnancy_testing',
                    'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition',
                    'prenatal_photo',
                    'resource', 'social_history', 'vitals', 'weight', 'acute_findings',
                    'acute_illness_danger_signs',
                    'acute_illness_follow_up', 'acute_illness_muac', 'acute_illness_nutrition',
                    'acute_illness_vitals',
                    'call_114', 'exposure', 'hc_contact', 'health_education', 'isolation',
                    'malaria_testing',
                    'medication_distribution', 'send_to_hc', 'symptoms_general', 'symptoms_gi',
                    'symptoms_respiratory',
                    'travel_history', 'treatment_history', 'treatment_ongoing',
                    'participant_consent', 'nutrition_caring',
                    'nutrition_contributing_factors', 'nutrition_feeding', 'nutrition_follow_up',
                    'nutrition_food_security',
                    'nutrition_health_education', 'nutrition_hygiene', 'nutrition_send_to_hc',
                    'appointment_confirmation',
                    'prenatal_follow_up', 'prenatal_send_to_hc', 'counseling_session')
    AND ((sess.field_session_target_id IS NOT NULL AND clinic.field_clinic_target_id IS NOT NULL) OR nutr.field_nutrition_encounter_target_id IS NOT NULL OR
         pren.field_prenatal_encounter_target_id IS NOT NULL OR
         acute.field_acute_illness_encounter_target_id IS NOT NULL)
    GROUP BY
      field_person_target_id
    HAVING
      COUNT(*) > 1
  )

  GROUP BY
    gender
  ORDER BY
    gender DESC;
END//

DELIMITER ;

SELECT 'Impacted patients report: ' AS '';
SELECT '* 0 - 1 month:' AS '';
CALL impacted_report('lt1m');
SELECT '* 1 month - 2 years:' AS '';
CALL impacted_report('lt2y');
SELECT '* 2 years - 5 years:' AS '';
CALL impacted_report('lt5y');
SELECT '* 5 years - 10 years:' AS '';
CALL impacted_report('lt10y');
SELECT '* 10 years - 20 years:' AS '';
CALL impacted_report('lt20y');
SELECT '* 20 years - 50 years:' AS '';
CALL impacted_report('lt50y');
SELECT '* Older than 50 years:' AS '';
CALL impacted_report('mt50y');

SELECT
  CONCAT("Patiens with missing AGE: ", COUNT(DISTINCT nid)) AS ''
FROM
  node
WHERE
    type = 'person'
AND nid NOT IN (SELECT
                  entity_id
                FROM
                  field_data_field_birth_date
                WHERE
                  field_birth_date_value IS NOT NULL)
AND nid NOT IN (SELECT
                  entity_id
                FROM
                  field_data_field_deleted
                WHERE
                  field_deleted_value = 1);
SELECT
  CONCAT("Patiens with missing GENDER: ", COUNT(DISTINCT nid)) AS ''
FROM
  node
WHERE
    type = 'person'
AND nid NOT IN (SELECT
                  entity_id
                FROM
                  field_data_field_gender
                WHERE
                  field_gender_value IS NOT NULL)
AND nid NOT IN (SELECT
                  entity_id
                FROM
                  field_data_field_deleted
                WHERE
                  field_deleted_value = 1);

DROP PROCEDURE IF EXISTS impacted_report;