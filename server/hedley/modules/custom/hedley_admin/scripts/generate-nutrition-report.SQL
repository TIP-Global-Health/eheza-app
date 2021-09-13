SELECT '----------------------' AS '';

SELECT concat(count(*), ' children with age below 6 years located') AS ''
FROM
  field_data_field_birth_date b
WHERE
    field_birth_date_value + INTERVAL 6 YEAR > CURDATE()
;

DROP PROCEDURE IF EXISTS malnutrition_calc_age;
DELIMITER //
CREATE PROCEDURE malnutrition_calc_age(IN type1 VARCHAR(20), IN type2 VARCHAR(20), IN zupper FLOAT, IN zlower FLOAT, IN description TEXT)
BEGIN
SELECT CONCAT(description, ': ', COUNT((field_zscore_age_value >= zlower AND field_zscore_age_value <= zupper) OR NULL)) AS ''  FROM
  field_data_field_zscore_age a LEFT JOIN
    field_data_field_date_measured ms ON a.entity_id = ms.entity_id LEFT JOIN
    node n ON n.nid=a.entity_id LEFT JOIN
    field_data_field_person p ON n.nid=p.entity_id LEFT JOIN
    field_data_field_deleted del ON p.field_person_target_id = del.entity_id
WHERE
  p.field_person_target_id is not null AND
  (del.field_deleted_value = 0 or del.field_deleted_value is null) AND
  (ms.field_date_measured_value + INTERVAL 1 YEAR > CURDATE()) AND
  n.type IN (type1, type2);
END//
DELIMITER ;

DROP PROCEDURE IF EXISTS malnutrition_calc_length;
DELIMITER //
CREATE PROCEDURE malnutrition_calc_length(IN type1 VARCHAR(20), IN type2 VARCHAR(20), IN zupper FLOAT, IN zlower FLOAT, IN description TEXT)
BEGIN
  SELECT CONCAT(description, ': ', COUNT((field_zscore_length_value > zlower AND field_zscore_length_value <= zupper) OR NULL)) AS '' FROM
    field_data_field_zscore_length a LEFT JOIN
      field_data_field_date_measured ms ON a.entity_id = ms.entity_id LEFT JOIN
      node n ON n.nid=a.entity_id LEFT JOIN
      field_data_field_person p ON n.nid=p.entity_id LEFT JOIN
      field_data_field_deleted del ON p.field_person_target_id = del.entity_id
  WHERE
    p.field_person_target_id is not null AND
    (del.field_deleted_value = 0 or del.field_deleted_value is null) AND
    (ms.field_date_measured_value + INTERVAL 1 YEAR > CURDATE()) AND
    n.type IN (type1, type2);
END//
DELIMITER ;

SELECT '----------------------' AS '';

SELECT 'Malnutrition report for past year:' AS '';
CALL malnutrition_calc_age('height', 'nutrition_height', -2.0, -3.0, 'Stunting Moderate (one visit, or more)');
CALL malnutrition_calc_age('height', 'nutrition_height', -3.0, -99999.0, 'Stunting Severe (one visit, or more)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -2.0, -3.0, 'Underweight Moderate (one visit, or more)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -3.0, -99999.0, 'Underweight Severe (one visit, or more)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -2.0, -3.0, 'Wasting Moderate (one visit, or more)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -3.0, -99999.0, 'Wasting Severe (one visit, or more)');
