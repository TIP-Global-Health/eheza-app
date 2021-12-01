SELECT 'Outcomes of completed pregnancies (30 days beyond EDD)' as '';
SELECT
  field_outcome_value, COUNT(*) AS ''
FROM
  field_data_field_outcome
WHERE field_outcome_value NOT IN ('referred-to-hc', 'illness-resolved')
GROUP BY
 field_outcome_value;

SELECT 'Outcome Location' AS '';
SELECT
  field_outcome_location_value, COUNT(*) AS ''
FROM
  field_data_field_outcome_location
GROUP BY
  field_outcome_location_value;
