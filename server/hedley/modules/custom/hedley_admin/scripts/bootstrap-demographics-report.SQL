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

# Impacted patients calculation.
DROP TABLE IF EXISTS person_impacted;
CREATE TABLE person_impacted
(
  `entity_id` int(10) UNSIGNED NOT NULL COMMENT 'The entity id this data is attached to'
);
ALTER TABLE person_impacted
  ADD PRIMARY KEY entity_id (entity_id);

INSERT
INTO
  person_impacted

  # This would narrow down the set of people to the ones who
  # completed at least two encounters.

SELECT
  p.field_person_target_id
FROM
  node ms
    LEFT JOIN field_data_field_person p ON p.entity_id = ms.nid
    LEFT JOIN field_data_field_session sess ON sess.entity_id = ms.nid
    LEFT JOIN field_data_field_clinic clinic ON sess.field_session_target_id = clinic.entity_id
    LEFT JOIN field_data_field_group_type gt ON field_clinic_target_id = gt.entity_id
    LEFT JOIN field_data_field_nutrition_encounter nutr ON nutr.entity_id = ms.nid
    LEFT JOIN field_data_field_prenatal_encounter pren ON pren.entity_id = ms.nid
    LEFT JOIN field_data_field_acute_illness_encounter acute ON acute.entity_id = ms.nid
WHERE
    ms.type IN ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors', 'core_physical_exam',
                'danger_signs', 'family_planning', 'follow_up', 'group_health_education', 'group_send_to_hc', 'height',
                'lactation', 'last_menstrual_period', 'medical_history', 'medication', 'mother_fbf', 'muac',
                'nutrition', 'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo',
                'nutrition_weight', 'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo',
                'pregnancy_testing', 'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition',
                'prenatal_photo', 'resource', 'social_history', 'vitals', 'weight', 'acute_findings',
                'acute_illness_danger_signs', 'acute_illness_follow_up', 'acute_illness_muac',
                'acute_illness_nutrition', 'acute_illness_vitals', 'call_114', 'exposure', 'hc_contact',
                'health_education', 'isolation', 'malaria_testing', 'medication_distribution', 'send_to_hc',
                'symptoms_general', 'symptoms_gi', 'symptoms_respiratory', 'travel_history', 'treatment_history',
                'treatment_ongoing', 'participant_consent', 'nutrition_caring', 'nutrition_contributing_factors',
                'nutrition_feeding', 'nutrition_follow_up', 'nutrition_food_security', 'nutrition_health_education',
                'nutrition_hygiene', 'nutrition_send_to_hc', 'appointment_confirmation', 'prenatal_follow_up',
                'prenatal_send_to_hc', 'counseling_session')
AND ((sess.field_session_target_id IS NOT NULL AND clinic.field_clinic_target_id IS NOT NULL) OR
     nutr.field_nutrition_encounter_target_id IS NOT NULL OR pren.field_prenatal_encounter_target_id IS NOT NULL OR
     acute.field_acute_illness_encounter_target_id IS NOT NULL)
AND (field_group_type_value IN ('pmtct', 'fbf', 'sorwathe', 'chw') OR sess.field_session_target_id IS NULL)
GROUP BY
  p.field_person_target_id
HAVING
      COUNT(DISTINCT field_session_target_id) + COUNT(DISTINCT field_nutrition_encounter_target_id) +
      COUNT(DISTINCT field_prenatal_encounter_target_id) + COUNT(DISTINCT field_acute_illness_encounter_target_id) > 1;