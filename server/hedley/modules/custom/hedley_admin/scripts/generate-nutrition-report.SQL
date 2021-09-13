SELECT '----------------------' AS '';

CREATE TABLE IF NOT EXISTS measurement_weight_counts
(
  `entity_id` int(10) UNSIGNED NOT NULL COMMENT 'The entity id this data is attached to',
  `count`     int
);
ALTER TABLE measurement_weight_counts
  ADD PRIMARY KEY entity_id (entity_id);

CREATE TABLE IF NOT EXISTS measurement_height_counts
(
  `entity_id` int(10) UNSIGNED NOT NULL COMMENT 'The entity id this data is attached to',
  `count`     int
);
ALTER TABLE measurement_height_counts
  ADD PRIMARY KEY entity_id (entity_id);

INSERT
INTO
  measurement_weight_counts
SELECT
  p.field_person_target_id,
  COUNT(*)
FROM
  field_data_field_zscore_age a
    LEFT JOIN
    field_data_field_date_measured ms ON a.entity_id = ms.entity_id
    LEFT JOIN
    node n ON n.nid = a.entity_id
    LEFT JOIN
    field_data_field_person p ON n.nid = p.entity_id
    LEFT JOIN
    field_data_field_deleted del ON p.field_person_target_id = del.entity_id
WHERE
  n.type IN ('weight', 'nutrition_weight')
GROUP BY
  p.field_person_target_id;

INSERT
INTO
  measurement_height_counts
SELECT
  p.field_person_target_id,
  COUNT(*)
FROM
  field_data_field_zscore_age a
    LEFT JOIN
    field_data_field_date_measured ms ON a.entity_id = ms.entity_id
    LEFT JOIN
    node n ON n.nid = a.entity_id
    LEFT JOIN
    field_data_field_person p ON n.nid = p.entity_id
    LEFT JOIN
    field_data_field_deleted del ON p.field_person_target_id = del.entity_id
WHERE
  n.type IN ('height', 'nutrition_height')
GROUP BY
  p.field_person_target_id;

SELECT CONCAT(COUNT(*), ' children with age below 6 years located') AS ''
FROM
  field_data_field_birth_date b
WHERE
    field_birth_date_value + INTERVAL 6 YEAR > CURDATE()
;

DROP PROCEDURE IF EXISTS malnutrition_calc_age;
DELIMITER //
CREATE PROCEDURE malnutrition_calc_age(IN type1 VARCHAR(20), IN type2 VARCHAR(20), IN zupper FLOAT, IN zlower FLOAT, IN min_visit_count INT, IN description TEXT)
BEGIN
  if type1 = 'weight' THEN
    SELECT CONCAT(description, ': ', COUNT((field_zscore_age_value >= zlower AND field_zscore_age_value <= zupper) OR NULL)) AS ''  FROM
      field_data_field_zscore_age a LEFT JOIN
        field_data_field_date_measured ms ON a.entity_id = ms.entity_id LEFT JOIN
        node n ON n.nid=a.entity_id LEFT JOIN
        field_data_field_person p ON n.nid=p.entity_id LEFT JOIN
        field_data_field_birth_date bdate ON p.field_person_target_id = bdate.entity_id LEFT JOIN
        field_data_field_deleted del ON p.field_person_target_id = del.entity_id LEFT JOIN
        measurement_weight_counts c ON c.entity_id = p.field_person_target_id
    WHERE
      p.field_person_target_id is not null AND
      (del.field_deleted_value = 0 or del.field_deleted_value is null) AND
      (ms.field_date_measured_value + INTERVAL 1 YEAR > CURDATE()) AND
      field_birth_date_value + INTERVAL 6 YEAR > CURDATE() AND
      n.type IN (type1, type2) AND
      c.count >= min_visit_count;
  ELSE
    SELECT CONCAT(description, ': ', COUNT((field_zscore_age_value >= zlower AND field_zscore_age_value <= zupper) OR NULL)) AS ''  FROM
      field_data_field_zscore_age a LEFT JOIN
        field_data_field_date_measured ms ON a.entity_id = ms.entity_id LEFT JOIN
        node n ON n.nid=a.entity_id LEFT JOIN
        field_data_field_person p ON n.nid=p.entity_id LEFT JOIN
        field_data_field_birth_date bdate ON p.field_person_target_id = bdate.entity_id LEFT JOIN
        field_data_field_deleted del ON p.field_person_target_id = del.entity_id LEFT JOIN
        measurement_height_counts c ON c.entity_id = p.field_person_target_id
    WHERE
      p.field_person_target_id is not null AND
      (del.field_deleted_value = 0 or del.field_deleted_value is null) AND
      (ms.field_date_measured_value + INTERVAL 1 YEAR > CURDATE()) AND
      field_birth_date_value + INTERVAL 6 YEAR > CURDATE() AND
      n.type IN (type1, type2) AND
      c.count >= min_visit_count;
  END IF;
END//
DELIMITER ;

DROP PROCEDURE IF EXISTS malnutrition_calc_length;
DELIMITER //
CREATE PROCEDURE malnutrition_calc_length(IN type1 VARCHAR(20), IN type2 VARCHAR(20), IN zupper FLOAT, IN zlower FLOAT, IN min_visit_count INT, IN description TEXT)
BEGIN
  SELECT CONCAT(description, ': ', COUNT((field_zscore_length_value > zlower AND field_zscore_length_value <= zupper) OR NULL)) AS '' FROM
    field_data_field_zscore_length a LEFT JOIN
      field_data_field_date_measured ms ON a.entity_id = ms.entity_id LEFT JOIN
      node n ON n.nid=a.entity_id LEFT JOIN
      field_data_field_person p ON n.nid=p.entity_id LEFT JOIN
      field_data_field_birth_date bdate ON p.field_person_target_id = bdate.entity_id LEFT JOIN
      field_data_field_deleted del ON p.field_person_target_id = del.entity_id LEFT JOIN
      measurement_weight_counts c ON c.entity_id = p.field_person_target_id
  WHERE
    p.field_person_target_id is not null AND
    (del.field_deleted_value = 0 or del.field_deleted_value is null) AND
    (ms.field_date_measured_value + INTERVAL 1 YEAR > CURDATE()) AND
    field_birth_date_value + INTERVAL 6 YEAR > CURDATE() AND
    n.type IN (type1, type2) AND
    c.count >= min_visit_count;
END//
DELIMITER ;

SELECT '----------------------' AS '';

SELECT 'Malnutrition report for past year:' AS '';

CALL malnutrition_calc_age('height', 'nutrition_height', -2.0, -3.0, 1, 'Stunting Moderate (one visit, or more)');
CALL malnutrition_calc_age('height', 'nutrition_height', -3.0, -99999.0, 1, 'Stunting Severe (one visit, or more)');
CALL malnutrition_calc_age('height', 'nutrition_height', -2.0, -3.0, 2, 'Stunting Moderate (more than one visit)');
CALL malnutrition_calc_age('height', 'nutrition_height', -3.0, -99999.0, 2, 'Stunting Severe (more than one visit)');
CALL malnutrition_calc_age('height', 'nutrition_height', -2.0, -3.0, 3, 'Stunting Moderate (more than two visits)');
CALL malnutrition_calc_age('height', 'nutrition_height', -3.0, -99999.0, 3, 'Stunting Severe (more than two visits)');

CALL malnutrition_calc_age('weight', 'nutrition_weight', -2.0, -3.0, 1, 'Underweight Moderate (one visit, or more)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -3.0, -99999.0, 1, 'Underweight Severe (one visit, or more)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -2.0, -3.0, 2, 'Underweight Moderate (more than one visit)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -3.0, -99999.0, 2, 'Underweight Severe (more than one visit)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -2.0, -3.0, 3, 'Underweight Moderate (more than two visits)');
CALL malnutrition_calc_age('weight', 'nutrition_weight', -3.0, -99999.0, 3, 'Underweight Severe (more than two visits)');

CALL malnutrition_calc_length('weight', 'nutrition_weight', -2.0, -3.0, 1, 'Wasting Moderate (one visit, or more)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -3.0, -99999.0, 1, 'Wasting Severe (one visit, or more)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -2.0, -3.0, 2, 'Wasting Moderate (more than one visit)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -3.0, -99999.0, 2, 'Wasting Severe (more than one visit)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -2.0, -3.0, 3, 'Wasting Moderate (more than two visits)');
CALL malnutrition_calc_length('weight', 'nutrition_weight', -3.0, -99999.0, 3, 'Wasting Severe (more than two visits)');

DROP PROCEDURE malnutrition_calc_length;
DROP PROCEDURE malnutrition_calc_age;
DROP TABLE measurement_weight_counts;
DROP TABLE measurement_height_counts;
