select 'Prenatal report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_prenatal_encounter_target_id)) AS '' FROM field_data_field_prenatal_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_prenatal_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;

SELECT 'Nutrition report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_nutrition_encounter_target_id)) AS '' FROM field_data_field_nutrition_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_nutrition_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;

SELECT 'Acute illness report:' AS '';
SELECT CONCAT('Completed encounters: ', COUNT(DISTINCT field_acute_illness_encounter_target_id)) AS '' FROM field_data_field_acute_illness_encounter;
SELECT CONCAT('Unique patients: ', COUNT(DISTINCT field_person_target_id)) AS '' FROM field_data_field_acute_illness_encounter e LEFT JOIN field_data_field_person p on e.entity_id=p.entity_id;
