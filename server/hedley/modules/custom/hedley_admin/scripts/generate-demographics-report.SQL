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

SELECT 'Impacted patients report: ' AS '';