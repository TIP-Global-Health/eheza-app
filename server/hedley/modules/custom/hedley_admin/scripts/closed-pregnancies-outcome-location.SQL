SELECT
  field_outcome_location_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome_location fol
    LEFT JOIN
    node ON fol.entity_id = node.nid
WHERE
    FROM_UNIXTIME(node.changed) < :limit
GROUP BY
  field_outcome_location_value