  SELECT
  (SELECT
     COUNT(*)
   FROM
     field_data_field_expected_date_concluded edd
       RIGHT JOIN
       field_data_field_individual_participant inp on inp.field_individual_participant_target_id = edd.entity_id
       RIGHT JOIN
       field_data_field_prenatal_encounter_type fdfpet ON inp.entity_id = fdfpet.entity_id
       LEFT JOIN
       field_data_field_encounter_type encounter ON edd.entity_id = encounter.entity_id
       LEFT JOIN
       node ON edd.entity_id = node.nid
   WHERE
       encounter.field_encounter_type_value = 'antenatal' AND
       edd.field_expected_date_concluded_value is NOT NULL AND
       fdfpet.field_prenatal_encounter_type_value='nurse' AND
       FROM_UNIXTIME(node.created) < :limit AND
       DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) > 180 AND
       DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) < 320) AS first_trimester,


   (SELECT
      COUNT(*)
    FROM
      field_data_field_expected_date_concluded edd
        RIGHT JOIN
      field_data_field_individual_participant inp on inp.field_individual_participant_target_id = edd.entity_id
        RIGHT JOIN
      field_data_field_prenatal_encounter_type fdfpet ON inp.entity_id = fdfpet.entity_id
        LEFT JOIN
        field_data_field_encounter_type encounter ON edd.entity_id = encounter.entity_id
        LEFT JOIN
        node ON edd.entity_id = node.nid
    WHERE
        encounter.field_encounter_type_value = 'antenatal' AND
        edd.field_expected_date_concluded_value is NOT NULL AND
        fdfpet.field_prenatal_encounter_type_value='nurse' AND
        FROM_UNIXTIME(node.created) < :limit AND
        DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) > 90 AND
        DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) <= 180) AS second_trimester,

    (SELECT
       COUNT(*)
     FROM
       field_data_field_expected_date_concluded edd
         RIGHT JOIN
         field_data_field_individual_participant inp on inp.field_individual_participant_target_id = edd.entity_id
         RIGHT JOIN
         field_data_field_prenatal_encounter_type fdfpet ON inp.entity_id = fdfpet.entity_id
         LEFT JOIN
         field_data_field_encounter_type encounter ON edd.entity_id = encounter.entity_id
         LEFT JOIN
         node ON edd.entity_id = node.nid
     WHERE
         encounter.field_encounter_type_value = 'antenatal' AND
         edd.field_expected_date_concluded_value is NOT NULL AND
         fdfpet.field_prenatal_encounter_type_value='nurse' AND
         FROM_UNIXTIME(node.created) < :limit AND
             DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) > 0 AND
             DATEDIFF (edd.field_expected_date_concluded_value, FROM_UNIXTIME(node.created)) <= 90) AS third_trimester


