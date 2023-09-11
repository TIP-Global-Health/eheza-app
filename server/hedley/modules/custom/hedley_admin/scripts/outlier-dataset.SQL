SELECT field_nurse_target_id                      AS interviewer_id,
       field_height_value                         AS height,
       field_weight_value                         AS weight,
       field_muac_value                           AS muac,
       field_zscore_age_value                     AS zscore_age,
       field_zscore_length_value                  AS zscore_length,
       field_zscore_bmi_value                     AS zscore_bmi
FROM field_data_field_height h
         LEFT JOIN field_data_field_nurse n
                   ON h.entity_id = n.entity_id
         LEFT JOIN field_data_field_nutrition_encounter neh
                   ON h.entity_id = neh.entity_id
         LEFT JOIN field_data_field_nutrition_encounter ecross
                   ON neh.field_nutrition_encounter_target_id = ecross.field_nutrition_encounter_target_id
         LEFT JOIN field_data_field_nutrition_encounter mcross
                   ON neh.field_nutrition_encounter_target_id = mcross.field_nutrition_encounter_target_id
         LEFT JOIN field_data_field_weight w
                   ON ecross.entity_id = w.entity_id
         LEFT JOIN field_data_field_zscore_age zage
                   ON w.entity_id = zage.entity_id
         LEFT JOIN field_data_field_zscore_length zlength
                   ON w.entity_id = zlength.entity_id
         LEFT JOIN field_data_field_zscore_bmi zbmi
                   ON w.entity_id = zbmi.entity_id
         LEFT JOIN field_data_field_muac muac
                   ON mcross.entity_id = muac.entity_id
WHERE field_weight_value IS NOT NULL
  and mcross.bundle = 'nutrition_muac'
  AND ecross.bundle = 'nutrition_weight'
GROUP BY ecross.field_nutrition_encounter_target_id