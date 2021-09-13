SELECT '----------------------' AS '';

SELECT concat(count(*), ' children with age below 6 years located') AS ''
FROM
  field_data_field_birth_date b
WHERE
    field_birth_date_value + INTERVAL 6 YEAR > CURDATE()
;