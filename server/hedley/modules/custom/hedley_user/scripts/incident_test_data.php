<?php

/**
 * @file
 * Test data definitions for incident recovery mechanism tests.
 *
 * Each test case contains a single entity to be created.
 * Tests are run in order, so dependencies (participant, encounter) are created
 * before the entities that reference them (measurements).
 */

/**
 * Generate a UUID v4.
 */
function generate_uuid() {
  return sprintf(
    '%04x%04x-%04x-%04x-%04x-%04x%04x%04x',
    mt_rand(0, 0xffff), mt_rand(0, 0xffff),
    mt_rand(0, 0xffff),
    mt_rand(0, 0x0fff) | 0x4000,
    mt_rand(0, 0x3fff) | 0x8000,
    mt_rand(0, 0xffff), mt_rand(0, 0xffff), mt_rand(0, 0xffff)
  );
}

/**
 * Get all test cases.
 *
 * @param string $person_uuid
 *   UUID of existing person.
 * @param string $nurse_uuid
 *   UUID of existing nurse.
 * @param string $health_center_uuid
 *   UUID of existing health center.
 *
 * @return array
 *   Array of test cases, keyed by test name.
 */
function get_test_cases($person_uuid, $nurse_uuid, $health_center_uuid) {
  $date = date('Y-m-d');

  // Generate UUIDs upfront so they can be referenced across test cases.
  $participant_uuid = generate_uuid();
  $encounter_uuid = generate_uuid();

  // Base measurement fields shared by all nutrition measurements.
  $base_measurement = [
    'person' => $person_uuid,
    'nutrition_encounter' => $encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $test_cases = [
    // First create the participant (dependency for encounter).
    'individual_participant_nutrition' => [
      'description' => 'IndividualEncounterParticipant for Nutrition',
      'entity' => [
        'person' => $person_uuid,
        'encounter_type' => 'nutrition',
        'expected' => [
          'value' => $date,
          'value2' => NULL,
        ],
        'expected_date_concluded' => NULL,
        'date_concluded' => NULL,
        'outcome' => NULL,
        'outcome_location' => NULL,
        'newborn' => NULL,
        'deleted' => FALSE,
        'type' => 'individual_participant',
        'uuid' => $participant_uuid,
        'status' => 1,
        'shard' => $health_center_uuid,
      ],
    ],

    // Then create the encounter (dependency for measurements).
    'nutrition_encounter' => [
      'description' => 'NutritionEncounter',
      'entity' => [
        'individual_participant' => $participant_uuid,
        'scheduled_date' => [
          'value' => $date,
          'value2' => NULL,
        ],
        'nutrition_encounter_type' => 'nurse',
        'deleted' => FALSE,
        'type' => 'nutrition_encounter',
        'uuid' => $encounter_uuid,
        'status' => 1,
        'shard' => $health_center_uuid,
      ],
    ],

    // Nutrition measurements.
    'nutrition_muac' => [
      'description' => 'Nutrition MUAC measurement',
      'entity' => array_merge($base_measurement, [
        'muac' => 21.0,
        'deleted' => FALSE,
        'type' => 'nutrition_muac',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_height' => [
      'description' => 'Nutrition Height measurement',
      'entity' => array_merge($base_measurement, [
        'height' => 75.5,
        'deleted' => FALSE,
        'type' => 'nutrition_height',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_weight' => [
      'description' => 'Nutrition Weight measurement',
      'entity' => array_merge($base_measurement, [
        'weight' => 8.5,
        'deleted' => FALSE,
        'type' => 'nutrition_weight',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_photo' => [
      'description' => 'Nutrition Photo measurement',
      'entity' => array_merge($base_measurement, [
        'photo' => 'https://example.com/photo.jpg',
        'deleted' => FALSE,
        'type' => 'nutrition_photo',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_nutrition' => [
      'description' => 'Nutrition Nutrition (signs) measurement',
      'entity' => array_merge($base_measurement, [
        'nutrition_signs' => ['none'],
        'nutrition_assesment' => ['none'],
        'deleted' => FALSE,
        'type' => 'nutrition_nutrition',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_send_to_hc' => [
      'description' => 'Nutrition SendToHC measurement',
      'entity' => array_merge($base_measurement, [
        'send_to_hc' => ['none'],
        'reason_not_sent_to_hc' => 'none',
        'deleted' => FALSE,
        'type' => 'nutrition_send_to_hc',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_health_education' => [
      'description' => 'Nutrition HealthEducation measurement',
      'entity' => array_merge($base_measurement, [
        'health_education_signs' => ['none'],
        'reason_not_given_education' => 'none',
        'deleted' => FALSE,
        'type' => 'nutrition_health_education',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_contributing_factors' => [
      'description' => 'Nutrition ContributingFactors measurement',
      'entity' => array_merge($base_measurement, [
        'contributing_factors_signs' => ['none'],
        'deleted' => FALSE,
        'type' => 'nutrition_contributing_factors',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_follow_up' => [
      'description' => 'Nutrition FollowUp measurement',
      'entity' => array_merge($base_measurement, [
        'follow_up_options' => ['1-w'],
        'nutrition_assesment' => ['assesment-underweight-moderate'],
        'nutrition_signs' => ['edema'],
        'date_concluded' => $date,
        'deleted' => FALSE,
        'type' => 'nutrition_follow_up',
        'uuid' => generate_uuid(),
      ]),
    ],

    'nutrition_ncda' => [
      'description' => 'Nutrition NCDA measurement',
      'entity' => array_merge($base_measurement, [
        'ncda_signs' => ['receive-vitamin-a'],
        'anc_visits_dates' => [$date],
        'receive_option' => 'yes',
        'stunting_level' => 'green',
        'birth_weight' => 3200.0,
        'weight' => 8.5,
        'muac' => 13.5,
        'deleted' => FALSE,
        'type' => 'nutrition_ncda',
        'uuid' => generate_uuid(),
      ]),
    ],

    // =========================================================================
    // PRENATAL ENCOUNTER AND MEASUREMENTS
    // =========================================================================

    // Prenatal participant.
    'individual_participant_prenatal' => [
      'description' => 'IndividualEncounterParticipant for Prenatal',
      'entity' => [
        'person' => $person_uuid,
        'encounter_type' => 'antenatal',
        'expected' => [
          'value' => $date,
          'value2' => NULL,
        ],
        'expected_date_concluded' => NULL,
        'date_concluded' => NULL,
        'outcome' => NULL,
        'outcome_location' => NULL,
        'newborn' => NULL,
        'deleted' => FALSE,
        'type' => 'individual_participant',
        'uuid' => generate_uuid(),
        'status' => 1,
        'shard' => $health_center_uuid,
      ],
    ],
  ];

  // Get the prenatal participant UUID we just created.
  $prenatal_participant_uuid = end($test_cases)['entity']['uuid'];
  $prenatal_encounter_uuid = generate_uuid();

  // Add prenatal encounter.
  $test_cases['prenatal_encounter'] = [
    'description' => 'PrenatalEncounter',
    'entity' => [
      'individual_participant' => $prenatal_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'prenatal_encounter_type' => 'nurse',
      'prenatal_diagnoses' => ['none'],
      'past_prenatal_diagnoses' => ['none'],
      'next_visit_date' => NULL,
      'deleted' => FALSE,
      'type' => 'prenatal_encounter',
      'uuid' => $prenatal_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  // Base prenatal measurement fields.
  $prenatal_base = [
    'person' => $person_uuid,
    'prenatal_encounter' => $prenatal_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  // Add all prenatal measurements.
  $prenatal_measurements = [
    'breast_exam' => [
      'description' => 'Prenatal Breast Exam',
      'fields' => [
        'breast' => ['normal'],
        'breast_self_exam' => TRUE,
        'discharge_type' => 'milky',
        'type' => 'breast_exam',
      ],
    ],
    'core_physical_exam' => [
      'description' => 'Prenatal Core Physical Exam',
      'fields' => [
        'head_hair' => ['normal'],
        'eyes' => ['normal'],
        'heart' => ['normal-rate-and-rhythm'],
        'heart_murmur' => FALSE,
        'neck' => ['normal'],
        'lungs' => ['normal'],
        'abdomen' => ['normal'],
        'hands' => ['normal'],
        'legs' => ['normal'],
        'type' => 'core_physical_exam',
      ],
    ],
    'danger_signs' => [
      'description' => 'Prenatal Danger Signs',
      'fields' => [
        'danger_signs' => ['none'],
        'postpartum_mother' => ['none'],
        'postpartum_child' => ['none'],
        'type' => 'danger_signs',
      ],
    ],
    'last_menstrual_period' => [
      'description' => 'Prenatal Last Menstrual Period',
      'fields' => [
        'last_menstrual_period' => $date,
        'confident' => FALSE,
        'not_confident_reason' => 'irregular-cycle',
        'confirmation' => TRUE,
        'late_first_visit_reason' => 'unaware-of-pregnancy',
        'weight' => 55.0,
        'type' => 'last_menstrual_period',
      ],
    ],
    'medical_history' => [
      'description' => 'Prenatal Medical History',
      'fields' => [
        'medical_history' => ['none'],
        'physical_condition_history' => ['none'],
        'infectious_disease_history' => ['none'],
        'mental_health_issues' => ['none'],
        'preeclampsia_in_family' => 'unknown',
        'type' => 'medical_history',
      ],
    ],
    'medication' => [
      'description' => 'Prenatal Medication',
      'fields' => [
        'medication' => ['none'],
        'hiv_treatment' => ['none'],
        'hypertension_treatment' => ['none'],
        'malaria_treatment' => ['none'],
        'anemia_treatment' => ['none'],
        'syphilis_treatment' => ['none'],
        'type' => 'medication',
      ],
    ],
    'obstetrical_exam' => [
      'description' => 'Prenatal Obstetrical Exam',
      'fields' => [
        'fundal_palpable' => TRUE,
        'fundal_height' => 25.0,
        'fetal_presentation' => 'cephalic',
        'fetal_movement' => TRUE,
        'fetal_heart_rate' => 140,
        'c_section_scar' => 'none',
        'type' => 'obstetrical_exam',
      ],
    ],
    'obstetric_history' => [
      'description' => 'Prenatal Obstetric History',
      'fields' => [
        'currently_pregnant' => TRUE,
        'term_pregnancy' => 0,
        'preterm_pregnancy' => 0,
        'stillbirths_at_term' => 0,
        'stillbirths_preterm' => 0,
        'abortions' => 0,
        'live_children' => 0,
        'type' => 'obstetric_history',
      ],
    ],
    'obstetric_history_step2' => [
      'description' => 'Prenatal Obstetric History Step 2',
      'fields' => [
        'c_sections' => 1,
        'c_section_reason' => ['previous-c-section'],
        'obstetric_history' => ['none'],
        'obstetric_history_step2' => ['none'],
        'previous_delivery' => ['c-section-in-past'],
        'previous_delivery_period' => ['less-than-18-month'],
        'type' => 'obstetric_history_step2',
      ],
    ],
    'prenatal_family_planning' => [
      'description' => 'Prenatal Family Planning',
      'fields' => [
        'family_planning_signs' => ['none'],
        'type' => 'prenatal_family_planning',
      ],
    ],
    'prenatal_nutrition' => [
      'description' => 'Prenatal Nutrition',
      'fields' => [
        'height' => 160.0,
        'weight' => 55.0,
        'muac' => 25.0,
        'type' => 'prenatal_nutrition',
      ],
    ],
    'resource' => [
      'description' => 'Prenatal Malaria Prevention (Resource)',
      'fields' => [
        'resources' => ['none'],
        'phase_recorded' => 'initial',
        'type' => 'resource',
      ],
    ],
    'social_history' => [
      'description' => 'Prenatal Social History',
      'fields' => [
        'social_history' => ['none'],
        'type' => 'social_history',
      ],
    ],
    'vitals' => [
      'description' => 'Prenatal Vitals',
      'fields' => [
        'sys' => 120.0,
        'dia' => 80.0,
        'heart_rate' => 72,
        'respiratory_rate' => 16,
        'body_temperature' => 36.5,
        'type' => 'vitals',
      ],
    ],
    'prenatal_photo' => [
      'description' => 'Prenatal Photo',
      'fields' => [
        'photo' => 'https://example.com/prenatal_photo.jpg',
        'type' => 'prenatal_photo',
      ],
    ],
    'birth_plan' => [
      'description' => 'Prenatal Birth Plan',
      'fields' => [
        'birth_plan_signs' => ['none'],
        'family_planning_signs' => ['none'],
        'type' => 'birth_plan',
      ],
    ],
    'pregnancy_testing' => [
      'description' => 'Prenatal Pregnancy Test',
      'fields' => [
        'urine_pregnancy_test' => 'positive',
        'type' => 'pregnancy_testing',
      ],
    ],
    'prenatal_health_education' => [
      'description' => 'Prenatal Health Education',
      'fields' => [
        'prenatal_health_education' => ['expectations', 'warning-signs'],
        'health_education_signs_ph2' => ['nausea-vomiting', 'leg-cramps'],
        'type' => 'prenatal_health_education',
      ],
    ],
    'prenatal_follow_up' => [
      'description' => 'Prenatal Follow Up',
      'fields' => [
        'follow_up_options' => ['1-w'],
        'prenatal_assesment' => 'high-risk',
        'date_concluded' => $date,
        'type' => 'prenatal_follow_up',
      ],
    ],
    'prenatal_send_to_hc' => [
      'description' => 'Prenatal Send To HC',
      'fields' => [
        'send_to_hc' => ['none'],
        'reason_not_sent_to_hc' => 'none',
        'referrals' => ['none'],
        'reasons_for_non_referrals' => ['none'],
        'type' => 'prenatal_send_to_hc',
      ],
    ],
    'appointment_confirmation' => [
      'description' => 'Prenatal Appointment Confirmation',
      'fields' => [
        'appointment_confirmation' => $date,
        'type' => 'appointment_confirmation',
      ],
    ],
    'prenatal_blood_gprs_test' => [
      'description' => 'Prenatal Blood Group/Rhesus Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['fasting-12h'],
        'originating_encounter' => $prenatal_encounter_uuid,
        'blood_group' => 'a',
        'rhesus' => 'positive',
        'type' => 'prenatal_blood_gprs_test',
      ],
    ],
    'prenatal_hemoglobin_test' => [
      'description' => 'Prenatal Hemoglobin Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'hemoglobin_count' => 12.5,
        'type' => 'prenatal_hemoglobin_test',
      ],
    ],
    'prenatal_hepatitis_b_test' => [
      'description' => 'Prenatal Hepatitis B Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'negative',
        'type' => 'prenatal_hepatitis_b_test',
      ],
    ],
    'prenatal_hiv_test' => [
      'description' => 'Prenatal HIV Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'negative',
        'hiv_signs' => ['none'],
        'type' => 'prenatal_hiv_test',
      ],
    ],
    'prenatal_malaria_test' => [
      'description' => 'Prenatal Malaria Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'negative',
        'blood_smear_result' => 'not-taken',
        'type' => 'prenatal_malaria_test',
      ],
    ],
    'prenatal_random_blood_sugar_test' => [
      'description' => 'Prenatal Random Blood Sugar Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'sugar_count' => 95.0,
        'type' => 'prenatal_random_blood_sugar_test',
      ],
    ],
    'prenatal_syphilis_test' => [
      'description' => 'Prenatal Syphilis Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'positive',
        'illness_symptoms' => ['genital-ulcer', 'rash'],
        'type' => 'prenatal_syphilis_test',
      ],
    ],
    'prenatal_urine_dipstick_test' => [
      'description' => 'Prenatal Urine Dipstick Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_variant' => 'short',
        'protein' => '0',
        'ph' => '7-0',
        'glucose' => '0',
        'leukocytes' => 'negative',
        'nitrite' => 'negative',
        'urobilinogen' => '0-2',
        'haemoglobin' => 'negative',
        'ketone' => 'negative',
        'bilirubin' => 'negative',
        'type' => 'prenatal_urine_dipstick_test',
      ],
    ],
    'prenatal_labs_results' => [
      'description' => 'Prenatal Labs Results',
      'fields' => [
        'performed_tests' => ['blood-gprs', 'hemoglobin'],
        'completed_tests' => ['blood-gprs', 'hemoglobin'],
        'date_concluded' => $date,
        'patient_notified' => TRUE,
        'tests_with_follow_up' => ['hemoglobin'],
        'review_state' => 'completed',
        'type' => 'prenatal_labs_results',
      ],
    ],
    'prenatal_medication_distribution' => [
      'description' => 'Prenatal Medication Distribution',
      'fields' => [
        'prescribed_medication' => ['iron', 'folicacid'],
        'non_administration_reason' => ['none'],
        'recommended_treatment' => ['treatment-hypertension-add-hctz'],
        'avoiding_guidance_reason' => ['patient-declined'],
        'reinforce_treatment_signs' => ['reinforce-treatment-folic-acid'],
        'type' => 'prenatal_medication_distribution',
      ],
    ],
    'prenatal_symptom_review' => [
      'description' => 'Prenatal Symptom Review',
      'fields' => [
        'prenatal_symptoms' => ['nausea', 'heartburn'],
        'prenatal_symptom_questions' => ['question-back-pain-severe'],
        'flank_pain_sign' => 'both-sides',
        'type' => 'prenatal_symptom_review',
      ],
    ],
    'prenatal_outside_care' => [
      'description' => 'Prenatal Outside Care',
      'fields' => [
        'outside_care_signs' => ['seen-at-health-center'],
        'prenatal_diagnoses' => ['diagnosis-gestational-diabetes'],
        'outside_care_medications' => ['medication-iron'],
        'type' => 'prenatal_outside_care',
      ],
    ],
    'prenatal_hiv_pcr_test' => [
      'description' => 'Prenatal HIV PCR Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'hiv_viral_load_status' => 'undetectable',
        'hiv_viral_load' => 50.0,
        'type' => 'prenatal_hiv_pcr_test',
      ],
    ],
    'prenatal_mental_health' => [
      'description' => 'Prenatal Mental Health',
      'fields' => [
        'mental_health_signs' => ['question1-never', 'question2-sometimes'],
        'specialist_at_hc' => TRUE,
        'type' => 'prenatal_mental_health',
      ],
    ],
    'prenatal_tetanus_immunisation' => [
      'description' => 'Prenatal Tetanus Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'prenatal_tetanus_immunisation',
      ],
    ],
    'prenatal_breastfeeding' => [
      'description' => 'Prenatal Breastfeeding',
      'fields' => [
        'breastfeeding_signs' => ['none'],
        'type' => 'prenatal_breastfeeding',
      ],
    ],
    'prenatal_gu_exam' => [
      'description' => 'Prenatal GU Exam',
      'fields' => [
        'vaginal_exam_signs' => ['normal'],
        'gu_exam_signs' => ['none'],
        'postpartum_healing_problem' => ['hematoma'],
        'type' => 'prenatal_gu_exam',
      ],
    ],
    'prenatal_speciality_care' => [
      'description' => 'Prenatal Speciality Care',
      'fields' => [
        'speciality_care_signs' => ['none'],
        'type' => 'prenatal_speciality_care',
      ],
    ],
    'prenatal_partner_hiv_test' => [
      'description' => 'Prenatal Partner HIV Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'negative',
        'hiv_signs' => ['none'],
        'type' => 'prenatal_partner_hiv_test',
      ],
    ],
    'prenatal_aspirin' => [
      'description' => 'Prenatal Aspirin',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_aspirin',
      ],
    ],
    'prenatal_calcium' => [
      'description' => 'Prenatal Calcium',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_calcium',
      ],
    ],
    'prenatal_fefol' => [
      'description' => 'Prenatal Fefol',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_fefol',
      ],
    ],
    'prenatal_folate' => [
      'description' => 'Prenatal Folate',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_folate',
      ],
    ],
    'prenatal_iron' => [
      'description' => 'Prenatal Iron',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_iron',
      ],
    ],
    'prenatal_mms' => [
      'description' => 'Prenatal MMS',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_mms',
      ],
    ],
    'prenatal_mebendazole' => [
      'description' => 'Prenatal Mebendazole',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'prenatal_mebendazole',
      ],
    ],
  ];

  // Add each prenatal measurement to test cases.
  foreach ($prenatal_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($prenatal_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  return $test_cases;
}