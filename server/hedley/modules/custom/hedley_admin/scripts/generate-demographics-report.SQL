SELECT '----------------------' AS '';

# Generate individual encounter report.
SELECT 'Prenatal report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_prenatal_encounter_target_id)) AS '' FROM field_data_field_prenatal_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_prenatal_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;

SELECT 'Nutrition report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_nutrition_encounter_target_id)) AS '' FROM field_data_field_nutrition_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_nutrition_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;

SELECT 'Acute illness report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_acute_illness_encounter_target_id)) AS '' FROM field_data_field_acute_illness_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_acute_illness_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;

# Classify by age and gender.
DROP TABLE if exists person_classified;
CREATE TABLE person_classified (
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  age varchar(10),
  gender varchar(10)
);
INSERT into person_classified select b.entity_id, 'lt1m' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 1 month > curdate();
INSERT into person_classified select b.entity_id, 'lt2y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 2 year > curdate() and field_birth_date_value + interval 1 month <= curdate();
INSERT into person_classified select b.entity_id, 'lt5y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 5 year > curdate() and field_birth_date_value + interval 2 year <= curdate();
INSERT into person_classified select b.entity_id, 'lt10y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 10 year > curdate() and field_birth_date_value + interval 5 year <= curdate();
INSERT into person_classified select b.entity_id, 'lt20y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 20 year > curdate() and field_birth_date_value + interval 10 year <= curdate();
INSERT into person_classified select b.entity_id, 'lt50y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 50 year > curdate() and field_birth_date_value + interval 20 year <= curdate();
INSERT into person_classified select b.entity_id, 'mt50y' as age, field_gender_value as gender from field_data_field_birth_date b left join field_data_field_gender g on b.entity_id=g.entity_id where field_birth_date_value + interval 50 year < curdate();
DELETE FROM person_classified WHERE entity_id IN (SELECT entity_id FROM field_data_field_deleted WHERE field_deleted_value=1);

SELECT '----------------------' AS '';

SELECT 'Registered patients report: ' AS '';
SELECT '* 0 - 1 month:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt1m' group by gender ORDER BY gender DESC;
SELECT '* 1 month - 2 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt2y' group by gender ORDER BY gender DESC;
SELECT '* 2 years - 5 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt5y' group by gender ORDER BY gender DESC;
SELECT '* 5 years - 10 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt10y' group by gender ORDER BY gender DESC;
SELECT '* 10 years - 20 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt20y' group by gender ORDER BY gender DESC;
SELECT '* 20 years - 50 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'lt50y' group by gender ORDER BY gender DESC;
SELECT '* Older than 50 years:' AS '';
SELECT gender AS '', count(*) as '' from person_classified where age = 'mt50y' group by gender ORDER BY gender DESC;

SELECT '----------------------' AS '';

SELECT 'Impacted patients report: ' AS '';

# not yet working
select field_group_type_value, count(*) from node ms left join field_data_field_session sess_rel on ms.nid=sess_rel.entity_id left join field_data_field_clinic c on sess_rel.field_session_target_id=c.entity_id left join field_data_field_group_type gt on field_clinic_target_id=gt.entity_id    where type in ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors', 'core_physical_exam', 'danger_signs', 'family_planning', 'follow_up', 'group_health_education', 'group_send_to_hc', 'height', 'lactation', 'last_menstrual_period', 'medical_history', 'medication', 'mother_fbf', 'muac', 'nutrition', 'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo', 'nutrition_weight', 'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo', 'pregnancy_testing', 'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition', 'prenatal_photo', 'resource', 'social_history', 'vitals', 'weight', 'acute_findings', 'acute_illness_danger_signs', 'acute_illness_follow_up', 'acute_illness_muac', 'acute_illness_nutrition', 'acute_illness_vitals', 'call_114', 'exposure', 'hc_contact', 'health_education', 'isolation', 'malaria_testing', 'medication_distribution', 'send_to_hc', 'symptoms_general', 'symptoms_gi', 'symptoms_respiratory', 'travel_history', 'treatment_history', 'treatment_ongoing', 'participant_consent', 'nutrition_caring', 'nutrition_contributing_factors', 'nutrition_feeding', 'nutrition_follow_up', 'nutrition_food_security', 'nutrition_health_education', 'nutrition_hygiene', 'nutrition_send_to_hc', 'appointment_confirmation', 'prenatal_follow_up', 'prenatal_send_to_hc', 'counseling_session') and  field_group_type_value is not null group by field_group_type_value;