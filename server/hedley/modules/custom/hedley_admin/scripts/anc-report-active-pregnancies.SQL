SELECT
  val,
  COUNT(*) AS counter
FROM
  (SELECT
     COUNT(*) AS val
   FROM
     field_data_field_individual_participant ip
       LEFT JOIN
       field_data_field_prenatal_encounter_type fdfpet ON ip.entity_id = fdfpet.entity_id
       LEFT JOIN
       field_data_field_expected_date_concluded edd ON ip.field_individual_participant_target_id=edd.entity_id
       LEFT JOIN
       node ON ip.entity_id = node.nid
       LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
       LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
   WHERE
       ip.bundle = 'prenatal_encounter' AND
       date(edd.field_expected_date_concluded_value) >= DATE_ADD(:limit, INTERVAL 30 DAY) AND
       edd.field_expected_date_concluded_value is NOT NULL AND
       FROM_UNIXTIME(node.created) < :limit
   GROUP BY
     field_individual_participant_target_id) a
GROUP BY
  val
