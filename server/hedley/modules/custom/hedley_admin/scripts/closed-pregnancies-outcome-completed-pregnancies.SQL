SELECT
  field_outcome_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome fo
    LEFT JOIN
    node ON fo.entity_id = node.nid
WHERE field_outcome_value NOT IN ('referred-to-hc', 'illness-resolved') AND
      FROM_UNIXTIME(node.changed) < :limit
GROUP BY
  field_outcome_value