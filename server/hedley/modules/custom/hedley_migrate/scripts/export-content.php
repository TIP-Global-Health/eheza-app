<?php

/**
 * @file
 * Populate the type for all clinics.
 *
 * NOTE: Method is OBSOLETE. Use hedley_admin/scripts/generate-sample-db.php.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/export-content.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// For sample db: Rukura, Rwankuba, Test.
$health_centers_data = [
  7091 => ['anonymize' => TRUE],
  7092 => ['anonymize' => TRUE],
  28589 => ['anonymize' => FALSE],
];

// In case we need to pull real files, make sure
// that destination directory is writable.
foreach ($health_centers_data as $health_center_data) {
  if (!$health_center_data['anonymize']) {
    $destination = drupal_get_path('module', 'hedley_migrate') . '/images';
    if (!is_writable($destination)) {
      drush_print("Destination folder for images $destination is not writable.");
      drush_print('Please run export again after fixing this problem.');
      exit;
    }
    break;
  }
}

drush_print('Starting export!');

$faker = hedley_faker_create();
$health_centers_ids = array_keys($health_centers_data);
$catchment_areas = [
  [
    'id',
    'title',
    'created',
  ],
];
$health_centers = [
  [
    'id',
    'title',
    'field_catchment_area',
    'created',
  ],
];
$villages = [
  [
    'id',
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
    'field_health_center',
    'created',
  ],
];
$groups = [
  [
    'id',
    'title',
    'field_group_type',
    'field_health_center',
    'created',
  ],
];
$nurses = [
  [
    'id',
    'title',
    'field_role',
    'field_health_centers',
    'field_villages',
    'field_pin_code',
    'created',
  ],
];
$group_encounters = [
  [
    'id',
    'field_clinic',
    'field_scheduled_date',
    'created',
  ],
];
$participants = [
  [
    'id',
    'field_person',
    'field_adult',
    'field_adult_activities',
    'field_expected',
    'field_clinic',
    'created',
  ],
];
$people = [
  [
    'id',
    'title',
    'field_first_name',
    'field_second_name',
    'field_gender',
    'field_birth_date',
    'field_health_center',
    'field_birth_date_estimated',
    'field_hmis_number',
    'field_marital_status',
    'field_education_level',
    'field_ubudehe',
    'field_province',
    'field_district',
    'field_sector',
    'field_cell',
    'field_village',
    'field_hiv_status',
    'field_mode_of_delivery',
    'field_number_of_children',
    'field_photo',
    'field_national_id_number',
    'field_phone_number',
    'created',
  ],
];
$relationships = [
  [
    'id',
    'field_person',
    'field_related_by',
    'field_related_to',
    'created',
  ],
];
$group_measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'created',
  'field_session',
];
$group_measurements = [
  'attendance' => [
    array_merge(
      $group_measurements_fields,
      ['field_attended']
    ),
  ],
  'child_fbf' => [
    array_merge(
      $group_measurements_fields,
      ['field_distributed_amount', 'field_distribution_notice']
    ),
  ],
  'family_planning' => [
    array_merge(
      $group_measurements_fields,
      ['field_family_planning_signs']
    ),
  ],
  'height' => [
    array_merge(
      $group_measurements_fields,
      ['field_height', 'field_zscore_age']
    ),
  ],
  'lactation' => [
    array_merge(
      $group_measurements_fields,
      ['field_lactation_signs']
    ),
  ],
  'mother_fbf' => [
    array_merge(
      $group_measurements_fields,
      ['field_distributed_amount', 'field_distribution_notice']
    ),
  ],
  'muac' => [
    array_merge(
      $group_measurements_fields,
      ['field_muac']
    ),
  ],
  'nutrition' => [
    array_merge(
      $group_measurements_fields,
      ['field_nutrition_signs']
    ),
  ],
  'photo' => [
    array_merge(
      $group_measurements_fields,
      ['field_photo']
    ),
  ],
  'weight' => [
    array_merge(
      $group_measurements_fields,
      [
        'field_weight',
        'field_bmi',
        'field_zscore_age',
        'field_zscore_length',
        'field_zscore_bmi',
      ]
    ),
  ],
];
$individual_participants = [
  [
    'id',
    'field_person',
    'field_expected',
    'field_encounter_type',
    'field_date_concluded',
    'field_outcome',
    'field_outcome_location',
    'field_expected_date_concluded',
    'created',
  ],
];
$acute_illness_encounters = [
  [
    'id',
    'field_individual_participant',
    'field_scheduled_date',
    'created',
    'field_acute_illness_diagnosis',
  ],
];
$acute_illness_measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'created',
  'field_acute_illness_encounter',
];
$acute_illness_measurements = [
  'acute_findings' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_findings_signs_general',
        'field_findings_signs_respiratory',
      ]
    ),
  ],
  'acute_illness_danger_signs' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_acute_illness_danger_signs',
      ]
    ),
  ],
  'acute_illness_muac' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_muac',
      ]
    ),
  ],
  'acute_illness_nutrition' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_nutrition_signs',
      ]
    ),
  ],
  'acute_illness_vitals' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_respiratory_rate',
        'field_body_temperature',
      ]
    ),
  ],
  'call_114' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_114_contact',
        'field_114_recommendation',
        'field_site_recommendation',
      ]
    ),
  ],
  'exposure' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_exposure',
      ]
    ),
  ],
  'hc_contact' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_hc_contact',
        'field_hc_recommendation',
        'field_hc_response_time',
        'field_ambulance_arrival_time',
      ]
    ),
  ],
  'health_education' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_health_education_signs',
      ]
    ),
  ],
  'isolation' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_isolation',
        'field_reason_for_not_isolating',
      ]
    ),
  ],
  'malaria_testing' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_malaria_rapid_test',
      ]
    ),
  ],
  'medication_distribution' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_prescribed_medication',
        'field_non_administration_reason',
      ]
    ),
  ],
  'send_to_hc' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_send_to_hc',
      ]
    ),
  ],
  'symptoms_general' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_fever_period',
        'field_chills_period',
        'field_night_sweats_period',
        'field_body_aches_period',
        'field_headache_period',
        'field_lethargy_period',
        'field_poor_suck_period',
        'field_unable_to_drink_period',
        'field_unable_to_eat_period',
        'field_increased_thirst_period',
        'field_dry_mouth_period',
        'field_severe_weakness_period',
        'field_yellow_eyes_period',
        'field_coke_colored_urine_period',
        'field_convulsions_period',
        'field_spontaneos_bleeding_period',
      ]
    ),
  ],
  'symptoms_gi' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_bloody_diarrhea_period',
        'field_non_bloody_diarrhea_period',
        'field_nausea_period',
        'field_vomiting_period',
        'field_abdominal_pain_period',
        'field_symptoms_gi_derived_signs',
      ]
    ),
  ],
  'symptoms_respiratory' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_cough_period',
        'field_shortness_of_breath_period',
        'field_nasal_congestion_period',
        'field_blood_in_sputum_period',
        'field_sore_throat_period',
        'field_loss_of_smell_period',
        'field_stabbing_chest_pain_period',
      ]
    ),
  ],
  'travel_history' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_travel_history',
      ]
    ),
  ],
  'treatment_history' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_treatment_history',
      ]
    ),
  ],
  'treatment_ongoing' => [
    array_merge(
      $acute_illness_measurements_fields,
      [
        'field_treatment_ongoing',
        'field_reason_for_not_taking',
        'field_missed_doses',
        'field_adverse_events',
      ]
    ),
  ],
];
$nutrition_encounters = [
  [
    'id',
    'field_individual_participant',
    'field_scheduled_date',
    'created',
  ],
];
$nutrition_measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'created',
  'field_nutrition_encounter',
];
$nutrition_measurements = [
  'nutrition_height' => [
    array_merge(
      $nutrition_measurements_fields,
      [
        'field_height',
        'field_zscore_age',
      ]
    ),
  ],
  'nutrition_muac' => [
    array_merge(
      $nutrition_measurements_fields,
      [
        'field_muac',
      ]
    ),
  ],
  'nutrition_nutrition' => [
    array_merge(
      $nutrition_measurements_fields,
      [
        'field_nutrition_signs',
      ]
    ),
  ],
  'nutrition_photo' => [
    array_merge(
      $nutrition_measurements_fields,
      [
        'field_photo',
      ]
    ),
  ],
  'nutrition_weight' => [
    array_merge(
      $nutrition_measurements_fields,
      [
        'field_weight',
        'field_bmi',
        'field_zscore_age',
        'field_zscore_length',
        'field_zscore_bmi',
      ]
    ),
  ],
];
$prenatal_encounters = [
  [
    'id',
    'field_individual_participant',
    'field_scheduled_date',
    'created',
  ],
];
$prenatal_measurements_fields = [
  'id',
  'field_person',
  'field_date_measured',
  'field_nurse',
  'created',
  'field_prenatal_encounter',
];
$prenatal_measurements = [
  'breast_exam' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_breast',
        'field_breast_self_exam',
      ]
    ),
  ],
  'core_physical_exam' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_head_hair',
        'field_eyes',
        'field_neck',
        'field_heart',
        'field_heart_murmur',
        'field_lungs',
        'field_abdomen',
        'field_hands',
        'field_legs',
      ]
    ),
  ],
  'danger_signs' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_danger_signs',
      ]
    ),
  ],
  'last_menstrual_period' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_last_menstrual_period',
        'field_confident',
      ]
    ),
  ],
  'medical_history' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_medical_history',
      ]
    ),
  ],
  'medication' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_medication',
      ]
    ),
  ],
  'obstetrical_exam' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_fundal_height',
        'field_fetal_presentation',
        'field_fetal_movement',
        'field_fetal_heart_rate',
        'field_c_section_scar',
      ]
    ),
  ],
  'obstetric_history' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_currently_pregnant',
        'field_term_pregnancy',
        'field_preterm_pregnancy',
        'field_stillbirths_at_term',
        'field_stillbirths_preterm',
        'field_abortions',
        'field_live_children',
      ]
    ),
  ],
  'obstetric_history_step2' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_c_sections',
        'field_c_section_reason',
        'field_previous_delivery_period',
        'field_obstetric_history',
        'field_previous_delivery',
      ]
    ),
  ],
  'prenatal_family_planning' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_family_planning_signs',
      ]
    ),
  ],
  'prenatal_nutrition' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_height',
        'field_weight',
        'field_muac',
      ]
    ),
  ],
  'prenatal_photo' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_photo',
      ]
    ),
  ],
  'resource' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_resources',
      ]
    ),
  ],
  'social_history' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_social_history',
        'field_partner_hiv_testing',
      ]
    ),
  ],
  'vitals' => [
    array_merge(
      $prenatal_measurements_fields,
      [
        'field_sys',
        'field_dia',
        'field_heart_rate',
        'field_respiratory_rate',
        'field_body_temperature',
      ]
    ),
  ],
];

$male_first_names = hedley_migrate_male_first_names();
$female_first_names = hedley_migrate_female_first_names();
$second_names = hedley_migrate_second_names();
$total_male_first_names = count($male_first_names);
$total_female_first_names = count($female_first_names);
$total_second_names = count($second_names);
$total_males = 0;
$total_females = 0;

$catchment_area_ids = [];
foreach ($health_centers_ids as $health_center_id) {
  $node = node_load($health_center_id);

  if (!$node) {
    drush_print("Error: can't find health center with ID $health_center_id!");
    exit;
  }

  $wrapper = entity_metadata_wrapper('node', $node);

  $catchment_area_id = $wrapper->field_catchment_area->getIdentifier();

  $health_centers[] = [
    $wrapper->getIdentifier(),
    str_replace(',', ' ', $wrapper->label()),
    $catchment_area_id,
    $node->created,
  ];

  if (!in_array($catchment_area_id, $catchment_area_ids)) {
    $wrapper = entity_metadata_wrapper('node', $catchment_area_id);
    $catchment_areas[] = [
      $wrapper->getIdentifier(),
      str_replace(',', ' ', $wrapper->label()),
      $wrapper->created->raw(),
    ];
    $catchment_area_ids[] = $catchment_area_id;
  }

  $villages_ids = hedley_migrate_resolve_for_export('village', 'field_health_center', [$health_center_id]);
  foreach ($villages_ids as $village_id) {
    $wrapper = entity_metadata_wrapper('node', $village_id);
    $villages[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_province->value(),
      $wrapper->field_district->value(),
      $wrapper->field_sector->value(),
      $wrapper->field_cell->value(),
      $wrapper->field_village->value(),
      $wrapper->field_health_center->getIdentifier(),
      $wrapper->created->raw(),
    ];
  }

  $groups_ids = hedley_migrate_resolve_for_export('clinic', 'field_health_center', [$health_center_id]);
  foreach ($groups_ids as $index => $group_id) {
    $wrapper = entity_metadata_wrapper('node', $group_id);
    $group_type = $wrapper->field_group_type->value();

    if ($group_type == 'chw') {
      // Chw groups automatically created during villages migration.
      // Therefore, we skip them here.
      unset($groups_ids[$index]);
      continue;
    }

    $groups[] = [
      $wrapper->getIdentifier(),
      str_replace(',', ' ', $wrapper->label()),
      $group_type,
      $wrapper->field_health_center->getIdentifier(),
      $wrapper->created->raw(),
    ];
  }
  $groups_ids = array_values($groups_ids);

  $nurses_ids = hedley_migrate_resolve_for_export('nurse', 'field_health_centers', [$health_center_id]);
  foreach ($nurses_ids as $nurse_id) {
    $wrapper = entity_metadata_wrapper('node', $nurse_id);

    $hc_ids = $wrapper->field_health_centers->value(['identifier' => TRUE]);
    foreach ($hc_ids as $key => $hc_id) {
      if (!in_array($hc_id, $health_centers_ids)) {
        unset($hc_ids[$key]);
      }
    }

    $villages_ids = $wrapper->field_villages->value(['identifier' => TRUE]);
    foreach ($villages_ids as $key => $village_id) {
      $village_wrapper = entity_metadata_wrapper('node', $village_id);
      $hc_id = $village_wrapper->field_health_center->getIdentifier();

      if (!in_array($hc_id, $health_centers_ids)) {
        unset($villages_ids[$key]);
      }
    }

    $nurses[$nurse_id] = [
      $nurse_id,
      str_replace(',', ' ', $wrapper->label()),
      implode('|', $wrapper->field_role->value()),
      implode('|', array_values($hc_ids)),
      implode('|', array_values($villages_ids)),
      $wrapper->field_pin_code->value(),
      $wrapper->created->raw(),
    ];
  }

  $group_encounters_ids = hedley_migrate_resolve_for_export('session', 'field_clinic', $groups_ids);
  foreach ($group_encounters_ids as $group_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $group_encounter_id);
    $group_encounters[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_clinic->getIdentifier(),
      hedley_migrate_export_date2_field($wrapper->field_scheduled_date->value(), TRUE),
      $wrapper->created->raw(),
    ];
  }

  $participants_ids = hedley_migrate_resolve_for_export('pmtct_participant', 'field_clinic', $groups_ids);
  $mothers_ids = $children_ids = [];
  foreach ($participants_ids as $participant_id) {
    $wrapper = entity_metadata_wrapper('node', $participant_id);
    $mothers_ids[] = $wrapper->field_adult->getIdentifier();
    $children_ids[] = $wrapper->field_person->getIdentifier();

    $participants[] = [
      $wrapper->getIdentifier(),
      $wrapper->field_person->getIdentifier(),
      $wrapper->field_adult->getIdentifier(),
      $wrapper->field_adult_activities->value(),
      hedley_migrate_export_date2_field($wrapper->field_expected->value()),
      $wrapper->field_clinic->getIdentifier(),
      $wrapper->created->raw(),
    ];
  }

  $unique_mothers_ids = array_unique($mothers_ids);
  $groups_patients_ids = array_merge($unique_mothers_ids, array_unique($children_ids));
  $health_center_patients_ids = hedley_migrate_resolve_for_export('person', 'field_health_center', [$health_center_id]);

  $people_ids = array_unique(array_merge($groups_patients_ids, $health_center_patients_ids));
  foreach ($people_ids as $person_id) {
    $person = node_load($person_id);
    $wrapper = entity_metadata_wrapper('node', $person);
    $gender = $wrapper->field_gender->value();
    $birth_date = $wrapper->field_birth_date->value();

    if ($health_centers_data[$health_center_id]['anonymize']) {
      if ($gender == 'male') {
        $total_males++;
        $first_name_id = ($total_males - 1) % $total_male_first_names;
        $second_name_id = (($total_males - 1) / $total_male_first_names) % $total_second_names;
        $first_name = $male_first_names[$first_name_id];
        $second_name = $second_names[$second_name_id];
      }
      else {
        $total_females++;
        $first_name_id = ($total_females - 1) % $total_female_first_names;
        $second_name_id = (($total_females - 1) / $total_female_first_names) % $total_second_names;
        $first_name = $female_first_names[$first_name_id];
        $second_name = $second_names[$second_name_id];
      }

      $national_id = '1199270' . $faker->numberBetween(100000000, 199999999);
      $phone_number = '0' . $faker->numberBetween(700000000, 799999999);
      $photo = hedley_migrate_allocate_photo_for_person($gender, $birth_date);
    }
    else {
      $name = hedley_person_get_paitent_name($person);
      $national_id = $wrapper->field_national_id_number->value();
      $phone_number = $wrapper->field_phone_number->value();
      $image = $wrapper->field_photo->value();
      $photo = empty($image) ? '' : hedley_migrate_export_real_image($image['fid'], $image['filename']);
    }

    $people[$person_id] = [
      $person_id,
      $name,
      $first_name,
      $second_name,
      $gender,
      hedley_migrate_export_date_field($birth_date),
      $wrapper->field_health_center->getIdentifier(),
      $wrapper->field_birth_date_estimated->value(),
      $wrapper->field_hmis_number->value(),
      $wrapper->field_marital_status->value(),
      $wrapper->field_education_level->value(),
      $wrapper->field_ubudehe->value(),
      $wrapper->field_province->value(),
      $wrapper->field_district->value(),
      $wrapper->field_sector->value(),
      $wrapper->field_cell->value(),
      $wrapper->field_village->value(),
      $wrapper->field_hiv_status->value(),
      $wrapper->field_mode_of_delivery->value(),
      $wrapper->field_number_of_children->value(),
      $photo,
      $national_id,
      $phone_number,
      $wrapper->created->raw(),
    ];
  }

  $relationships_ids = hedley_migrate_resolve_for_export('relationship', 'field_person', $unique_mothers_ids);
  foreach ($relationships_ids as $relationship_id) {
    $wrapper = entity_metadata_wrapper('node', $relationship_id);

    $relationships[$relationship_id] = [
      $relationship_id,
      $wrapper->field_person->getIdentifier(),
      $wrapper->field_related_by->value(),
      $wrapper->field_related_to->getIdentifier(),
      $wrapper->created->raw(),
    ];
  }

  foreach ($group_measurements as $type => $values) {
    $ids = hedley_migrate_resolve_for_export($type, 'field_session', $group_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        hedley_migrate_export_date_field($wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->created->raw(),
        $wrapper->field_session->getIdentifier(),
      ];

      switch ($type) {
        case 'attendance':
          $type_based_values = [$wrapper->field_attended->value()];
          break;

        case 'child_fbf':
        case 'mother_fbf':
          $type_based_values = [
            $wrapper->field_distributed_amount->value(),
            $wrapper->field_distribution_notice->value(),
          ];
          break;

        case 'family_planning':
          $type_based_values = [
            implode('|', $wrapper->field_family_planning_signs->value()),
          ];
          break;

        case 'height':
          $type_based_values = [
            $wrapper->field_height->value(),
            $wrapper->field_zscore_age->value(),
          ];
          break;

        case 'lactation':
          $type_based_values = [
            implode('|', $wrapper->field_lactation_signs->value()),
          ];
          break;

        case 'muac':
          $type_based_values = [$wrapper->field_muac->value()];
          break;

        case 'nutrition':
          $type_based_values = [
            implode('|', $wrapper->field_nutrition_signs->value()),
          ];
          break;

        case 'photo':
          if ($health_centers_data[$health_center_id]['anonymize']) {
            $gender = $wrapper->field_person->field_gender->value();
            $birth_date = $wrapper->field_person->field_birth_date->value();
            $photo = hedley_migrate_allocate_photo_for_person($gender, $birth_date);
          }
          else {
            $image = $wrapper->field_photo->value();
            $photo = empty($image) ? '' : hedley_migrate_export_real_image($image['fid'], $image['filename']);
          }
          $type_based_values = [$photo];
          break;

        case 'weight':
          $type_based_values = [
            $wrapper->field_weight->value(),
            $wrapper->field_bmi->value(),
            $wrapper->field_zscore_age->value(),
            $wrapper->field_zscore_length->value(),
            $wrapper->field_zscore_bmi->value(),
          ];
          break;

        default:
          $type_based_values = [];
      }

      $group_measurements[$type][] = array_merge($common_values, $type_based_values);
    }
  }

  // Handling individual encounters and measurements.
  if (empty($health_center_patients_ids)) {
    // There are no patients assigned to health center.
    // Skip to next health center.
    continue;
  }

  $individual_participants_ids = hedley_migrate_resolve_for_export('individual_participant', 'field_person', [$health_center_patients_ids]);
  if (empty($individual_participants_ids)) {
    // There are no individual participations for patients assigned
    // to health center. Skip to next health center.
    continue;
  }

  // Export individual participants.
  foreach ($individual_participants_ids as $individual_participant_id) {
    $wrapper = entity_metadata_wrapper('node', $individual_participant_id);

    $individual_participants[$individual_participant_id] = [
      $individual_participant_id,
      $wrapper->field_person->getIdentifier(),
      hedley_migrate_export_date2_field($wrapper->field_expected->value()),
      $wrapper->field_encounter_type->value(),
      hedley_migrate_export_date_field($wrapper->field_date_concluded->value()),
      $wrapper->field_outcome->value(),
      $wrapper->field_outcome_location->value(),
      hedley_migrate_export_date_field($wrapper->field_expected_date_concluded->value()),
      $wrapper->created->raw(),
    ];
  }

  // Export acute illness encounters.
  $acute_illness_encounters_ids = hedley_migrate_resolve_for_export('acute_illness_encounter', 'field_individual_participant', [$individual_participants_ids]);
  foreach ($acute_illness_encounters_ids as $acute_illness_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $acute_illness_encounter_id);

    $acute_illness_encounters[$acute_illness_encounter_id] = [
      $acute_illness_encounter_id,
      $wrapper->field_individual_participant->getIdentifier(),
      hedley_migrate_export_date2_field($wrapper->field_scheduled_date->value()),
      $wrapper->created->raw(),
      $wrapper->field_acute_illness_diagnosis->value(),
    ];
  }

  // Export acute illness measurements.
  foreach ($acute_illness_measurements as $type => $values) {
    $ids = hedley_migrate_resolve_for_export($type, 'field_acute_illness_encounter', $acute_illness_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        hedley_migrate_export_date_field($wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->created->raw(),
        $wrapper->field_acute_illness_encounter->getIdentifier(),
      ];

      switch ($type) {
        case 'acute_findings':
          $type_based_values = [
            implode('|', $wrapper->field_findings_signs_general->value()),
            implode('|', $wrapper->field_findings_signs_respiratory->value()),
          ];
          break;

        case 'acute_illness_danger_signs':
          $type_based_values = [
            implode('|', $wrapper->field_acute_illness_danger_signs->value()),
          ];
          break;

        case 'acute_illness_muac':
          $type_based_values = [
            $wrapper->field_muac->value(),
          ];
          break;

        case 'acute_illness_nutrition':
          $type_based_values = [
            implode('|', $wrapper->field_nutrition_signs->value()),
          ];
          break;

        case 'acute_illness_vitals':
          $type_based_values = [
            $wrapper->field_respiratory_rate->value(),
            $wrapper->field_body_temperature->value(),
          ];
          break;

        case 'call_114':
          $type_based_values = [
            implode('|', $wrapper->field_114_contact->value()),
            implode('|', $wrapper->field_114_recommendation->value()),
            implode('|', $wrapper->field_site_recommendation->value()),
          ];
          break;

        case 'exposure':
          $type_based_values = [
            implode('|', $wrapper->field_exposure->value()),
          ];
          break;

        case 'hc_contact':
          $type_based_values = [
            implode('|', $wrapper->field_hc_contact->value()),
            implode('|', $wrapper->field_hc_recommendation->value()),
            implode('|', $wrapper->field_hc_response_time->value()),
            implode('|', $wrapper->field_ambulance_arrival_time->value()),
          ];
          break;

        case 'health_education':
          $type_based_values = [
            implode('|', $wrapper->field_health_education_signs->value()),
          ];
          break;

        case 'isolation':
          $type_based_values = [
            implode('|', $wrapper->field_isolation->value()),
            implode('|', $wrapper->field_reason_for_not_isolating->value()),
          ];
          break;

        case 'malaria_testing':
          $type_based_values = [
            $wrapper->field_malaria_rapid_test->value(),
          ];
          break;

        case 'medication_distribution':
          $type_based_values = [
            implode('|', $wrapper->field_prescribed_medication->value()),
            implode('|', $wrapper->field_non_administration_reason->value()),
          ];
          break;

        case 'send_to_hc':
          $type_based_values = [
            implode('|', $wrapper->field_send_to_hc->value()),
          ];
          break;

        case 'symptoms_general':
          $type_based_values = [
            $wrapper->field_fever_period->value(),
            $wrapper->field_chills_period->value(),
            $wrapper->field_night_sweats_period->value(),
            $wrapper->field_body_aches_period->value(),
            $wrapper->field_headache_period->value(),
            $wrapper->field_lethargy_period->value(),
            $wrapper->field_poor_suck_period->value(),
            $wrapper->field_unable_to_drink_period->value(),
            $wrapper->field_unable_to_eat_period->value(),
            $wrapper->field_increased_thirst_period->value(),
            $wrapper->field_dry_mouth_period->value(),
            $wrapper->field_severe_weakness_period->value(),
            $wrapper->field_yellow_eyes_period->value(),
            $wrapper->field_coke_colored_urine_period->value(),
            $wrapper->field_convulsions_period->value(),
            $wrapper->field_spontaneos_bleeding_period->value(),
          ];
          break;

        case 'symptoms_gi':
          $type_based_values = [
            $wrapper->field_bloody_diarrhea_period->value(),
            $wrapper->field_non_bloody_diarrhea_period->value(),
            $wrapper->field_nausea_period->value(),
            $wrapper->field_vomiting_period->value(),
            $wrapper->field_abdominal_pain_period->value(),
            implode('|', $wrapper->field_symptoms_gi_derived_signs->value()),
          ];
          break;

        case 'symptoms_respiratory':
          $type_based_values = [
            $wrapper->field_cough_period->value(),
            $wrapper->field_shortness_of_breath_period->value(),
            $wrapper->field_nasal_congestion_period->value(),
            $wrapper->field_blood_in_sputum_period->value(),
            $wrapper->field_sore_throat_period->value(),
            $wrapper->field_loss_of_smell_period->value(),
            $wrapper->field_stabbing_chest_pain_period->value(),
          ];
          break;

        case 'travel_history':
          $type_based_values = [
            implode('|', $wrapper->field_travel_history->value()),
          ];
          break;

        case 'treatment_history':
          $type_based_values = [
            implode('|', $wrapper->field_treatment_history->value()),
          ];
          break;

        case 'treatment_ongoing':
          $type_based_values = [
            implode('|', $wrapper->field_treatment_ongoing->value()),
            $wrapper->field_reason_for_not_taking->value(),
            $wrapper->field_missed_doses->value(),
            implode('|', $wrapper->field_adverse_events->value()),
          ];
          break;

        default:
          $type_based_values = [];
      }

      $acute_illness_measurements[$type][] = array_merge($common_values, $type_based_values);
    }
  }

  // Export nutrition encounters.
  $nutrition_encounters_ids = hedley_migrate_resolve_for_export('nutrition_encounter', 'field_individual_participant', [$individual_participants_ids]);
  foreach ($nutrition_encounters_ids as $nutrition_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $nutrition_encounter_id);

    $nutrition_encounters[$nutrition_encounter_id] = [
      $nutrition_encounter_id,
      $wrapper->field_individual_participant->getIdentifier(),
      hedley_migrate_export_date2_field($wrapper->field_scheduled_date->value()),
      $wrapper->created->raw(),
    ];
  }

  // Export nutrition measurements.
  foreach ($nutrition_measurements as $type => $values) {
    $ids = hedley_migrate_resolve_for_export($type, 'field_nutrition_encounter', $nutrition_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        hedley_migrate_export_date_field($wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->created->raw(),
        $wrapper->field_nutrition_encounter->getIdentifier(),
      ];

      switch ($type) {
        case 'nutrition_height':
          $type_based_values = [
            $wrapper->field_height->value(),
            $wrapper->field_zscore_age->value(),
          ];
          break;

        case 'nutrition_muac':
          $type_based_values = [
            $wrapper->field_muac->value(),
          ];
          break;

        case 'nutrition_nutrition':
          $type_based_values = [
            implode('|', $wrapper->field_nutrition_signs->value()),
          ];
          break;

        case 'nutrition_photo':
          if ($health_centers_data[$health_center_id]['anonymize']) {
            $gender = $wrapper->field_person->field_gender->value();
            $birth_date = $wrapper->field_person->field_birth_date->value();
            $photo = hedley_migrate_allocate_photo_for_person($gender, $birth_date);
          }
          else {
            $image = $wrapper->field_photo->value();
            $photo = empty($image) ? '' : hedley_migrate_export_real_image($image['fid'], $image['filename']);
          }
          $type_based_values = [$photo];
          break;

        case 'nutrition_weight':
          $type_based_values = [
            $wrapper->field_weight->value(),
            $wrapper->field_bmi->value(),
            $wrapper->field_zscore_age->value(),
            $wrapper->field_zscore_length->value(),
            $wrapper->field_zscore_bmi->value(),
          ];
          break;

        default:
          $type_based_values = [];
      }

      $nutrition_measurements[$type][] = array_merge($common_values, $type_based_values);
    }
  }

  // Export prenatal encounters.
  $prenatal_encounters_ids = hedley_migrate_resolve_for_export('prenatal_encounter', 'field_individual_participant', [$individual_participants_ids]);
  foreach ($prenatal_encounters_ids as $prenatal_encounter_id) {
    $wrapper = entity_metadata_wrapper('node', $prenatal_encounter_id);

    $prenatal_encounters[$prenatal_encounter_id] = [
      $prenatal_encounter_id,
      $wrapper->field_individual_participant->getIdentifier(),
      hedley_migrate_export_date2_field($wrapper->field_scheduled_date->value()),
      $wrapper->created->raw(),
    ];
  }

  // Export prenatal measurements.
  foreach ($prenatal_measurements as $type => $values) {
    $ids = hedley_migrate_resolve_for_export($type, 'field_prenatal_encounter', $prenatal_encounters_ids);

    foreach ($ids as $id) {
      $wrapper = entity_metadata_wrapper('node', $id);
      $common_values = [
        $wrapper->getIdentifier(),
        $wrapper->field_person->getIdentifier(),
        hedley_migrate_export_date_field($wrapper->field_date_measured->value()),
        $wrapper->field_nurse->getIdentifier(),
        $wrapper->created->raw(),
        $wrapper->field_prenatal_encounter->getIdentifier(),
      ];

      switch ($type) {
        case 'breast_exam':
          $type_based_values = [
            implode('|', $wrapper->field_breast->value()),
            $wrapper->field_breast_self_exam->value(),
          ];
          break;

        case 'core_physical_exam':
          $type_based_values = [
            implode('|', $wrapper->field_head_hair->value()),
            implode('|', $wrapper->field_eyes->value()),
            implode('|', $wrapper->field_neck->value()),
            implode('|', $wrapper->field_heart->value()),
            $wrapper->field_heart_murmur->value(),
            implode('|', $wrapper->field_lungs->value()),
            implode('|', $wrapper->field_abdomen->value()),
            implode('|', $wrapper->field_hands->value()),
            implode('|', $wrapper->field_legs->value()),
          ];
          break;

        case 'danger_signs':
          $type_based_values = [
            implode('|', $wrapper->field_danger_signs->value()),
          ];
          break;

        case 'last_menstrual_period':
          $type_based_values = [
            hedley_migrate_export_date_field($wrapper->field_last_menstrual_period->value()),
            $wrapper->field_confident->value(),
          ];
          break;

        case 'medical_history':
          $type_based_values = [
            implode('|', $wrapper->field_medical_history->value()),
          ];
          break;

        case 'medication':
          $type_based_values = [
            implode('|', $wrapper->field_medication->value()),
          ];
          break;

        case 'obstetrical_exam':
          $type_based_values = [
            $wrapper->field_fundal_height->value(),
            $wrapper->field_fetal_presentation->value(),
            $wrapper->field_fetal_movement->value(),
            $wrapper->field_fetal_heart_rate->value(),
            $wrapper->field_c_section_scar->value(),
          ];
          break;

        case 'obstetric_history':
          $type_based_values = [
            $wrapper->field_currently_pregnant->value(),
            $wrapper->field_term_pregnancy->value(),
            $wrapper->field_preterm_pregnancy->value(),
            $wrapper->field_stillbirths_at_term->value(),
            $wrapper->field_stillbirths_preterm->value(),
            $wrapper->field_abortions->value(),
            $wrapper->field_live_children->value(),
          ];
          break;

        case 'obstetric_history_step2':
          $type_based_values = [
            $wrapper->field_c_sections->value(),
            implode('|', $wrapper->field_c_section_reason->value()),
            implode('|', $wrapper->field_previous_delivery_period->value()),
            implode('|', $wrapper->field_obstetric_history->value()),
            implode('|', $wrapper->field_previous_delivery->value()),
          ];
          break;

        case 'prenatal_family_planning':
          $type_based_values = [
            implode('|', $wrapper->field_family_planning_signs->value()),
          ];
          break;

        case 'prenatal_nutrition':
          $type_based_values = [
            $wrapper->field_height->value(),
            $wrapper->field_height->value(),
            $wrapper->field_height->value(),
          ];
          break;

        case 'prenatal_photo':
          if ($health_centers_data[$health_center_id]['anonymize']) {
            $gender = $wrapper->field_person->field_gender->value();
            $birth_date = $wrapper->field_person->field_birth_date->value();
            $photo = hedley_migrate_allocate_photo_for_person($gender, $birth_date);
          }
          else {
            $image = $wrapper->field_photo->value();
            $photo = empty($image) ? '' : hedley_migrate_export_real_image($image['fid'], $image['filename']);
          }
          $type_based_values = [$photo];
          break;

        case 'resource':
          $type_based_values = [
            implode('|', $wrapper->field_resources->value()),
          ];
          break;

        case 'social_history':
          $type_based_values = [
            implode('|', $wrapper->field_social_history->value()),
            $wrapper->field_partner_hiv_testing->value(),
          ];
          break;

        case 'vitals':
          $type_based_values = [
            $wrapper->field_sys->value(),
            $wrapper->field_dia->value(),
            $wrapper->field_heart_rate->value(),
            $wrapper->field_respiratory_rate->value(),
            $wrapper->field_body_temperature->value(),
          ];
          break;

        default:
          $type_based_values = [];
      }

      $prenatal_measurements[$type][] = array_merge($common_values, $type_based_values);
    }
  }
}

$mapping = [
  'catchment_area' => $catchment_areas,
  'health_center' => $health_centers,
  'village' => $villages,
  'clinic' => $groups,
  'nurse' => array_values($nurses),
  'session' => $group_encounters,
  'pmtct_participant' => $participants,
  'person' => array_values($people),
  'relationship' => array_values($relationships),
  'individual_participant' => array_values($individual_participants),
  'acute_illness_encounter' => array_values($acute_illness_encounters),
  'nutrition_encounter' => array_values($nutrition_encounters),
  'prenatal_encounter' => array_values($prenatal_encounters),
];

// Measurements mapping.
foreach (array_keys($group_measurements) as $type) {
  $mapping[$type] = $group_measurements[$type];
}
foreach (array_keys($acute_illness_measurements) as $type) {
  $mapping[$type] = $acute_illness_measurements[$type];
}
foreach (array_keys($nutrition_measurements) as $type) {
  $mapping[$type] = $nutrition_measurements[$type];
}
foreach (array_keys($prenatal_measurements) as $type) {
  $mapping[$type] = $prenatal_measurements[$type];
}

foreach ($mapping as $name => $rows) {
  $content = [];
  foreach ($rows as $row) {
    $content[] = implode(',', $row);
  }

  $path = drupal_get_path('module', 'hedley_migrate') . '/csv';
  $fp = fopen("$path/$name.csv", 'w');
  fwrite($fp, implode(PHP_EOL, $content));

  fclose($fp);
}

drush_print('Done!');
