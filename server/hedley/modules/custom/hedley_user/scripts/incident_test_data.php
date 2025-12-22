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

  // =========================================================================
  // ACUTE ILLNESS ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $acute_illness_participant_uuid = generate_uuid();
  $test_cases['individual_participant_acute_illness'] = [
    'description' => 'IndividualEncounterParticipant for Acute Illness',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'acute-illness',
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
      'uuid' => $acute_illness_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $acute_illness_encounter_uuid = generate_uuid();
  $test_cases['acute_illness_encounter'] = [
    'description' => 'AcuteIllnessEncounter',
    'entity' => [
      'individual_participant' => $acute_illness_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'sequence_number' => 1,
      'ai_encounter_type' => 'nurse-encounter',
      'acute_illness_diagnosis' => 'no-acute-illness-diagnosis',
      'deleted' => FALSE,
      'type' => 'acute_illness_encounter',
      'uuid' => $acute_illness_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $acute_illness_base = [
    'person' => $person_uuid,
    'acute_illness_encounter' => $acute_illness_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $acute_illness_measurements = [
    'symptoms_general' => [
      'description' => 'Acute Illness Symptoms General',
      'fields' => [
        'fever_period' => 2,
        'chills_period' => 1,
        'night_sweats_period' => 0,
        'body_aches_period' => 1,
        'headache_period' => 2,
        'coke_colored_urine_period' => 0,
        'convulsions_period' => 0,
        'dry_mouth_period' => 1,
        'increased_thirst_period' => 1,
        'lethargy_period' => 0,
        'poor_suck_period' => 0,
        'severe_weakness_period' => 0,
        'spontaneos_bleeding_period' => 0,
        'unable_to_drink_period' => 0,
        'unable_to_eat_period' => 0,
        'yellow_eyes_period' => 0,
        'type' => 'symptoms_general',
      ],
    ],
    'symptoms_respiratory' => [
      'description' => 'Acute Illness Symptoms Respiratory',
      'fields' => [
        'cough_period' => 3,
        'shortness_of_breath_period' => 1,
        'nasal_congestion_period' => 2,
        'blood_in_sputum_period' => 0,
        'sore_throat_period' => 1,
        'loss_of_smell_period' => 0,
        'stabbing_chest_pain_period' => 0,
        'type' => 'symptoms_respiratory',
      ],
    ],
    'symptoms_gi' => [
      'description' => 'Acute Illness Symptoms GI',
      'fields' => [
        'bloody_diarrhea_period' => 0,
        'non_bloody_diarrhea_period' => 2,
        'nausea_period' => 1,
        'vomiting_period' => 1,
        'abdominal_pain_period' => 1,
        'symptoms_gi_derived_signs' => ['intractable-vomiting'],
        'type' => 'symptoms_gi',
      ],
    ],
    'acute_illness_vitals' => [
      'description' => 'Acute Illness Vitals',
      'fields' => [
        'sys' => 120.0,
        'dia' => 80.0,
        'heart_rate' => 88,
        'respiratory_rate' => 20,
        'body_temperature' => 38.5,
        'type' => 'acute_illness_vitals',
      ],
    ],
    'acute_findings' => [
      'description' => 'Acute Illness Findings',
      'fields' => [
        'findings_signs_general' => ['sunken-eyes', 'poor-skin-turgor'],
        'findings_signs_respiratory' => ['nasal-flaring'],
        'type' => 'acute_findings',
      ],
    ],
    'malaria_testing' => [
      'description' => 'Acute Illness Malaria Testing',
      'fields' => [
        'malaria_rapid_test' => 'positive',
        'type' => 'malaria_testing',
      ],
    ],
    'covid_testing' => [
      'description' => 'Acute Illness COVID Testing',
      'fields' => [
        'rapid_test_result' => 'negative',
        'administration_note' => 'administered-today',
        'type' => 'covid_testing',
      ],
    ],
    'send_to_hc' => [
      'description' => 'Acute Illness Send To HC',
      'fields' => [
        'send_to_hc' => ['refer-to-hc'],
        'reason_not_sent_to_hc' => 'none',
        'type' => 'send_to_hc',
      ],
    ],
    'medication_distribution' => [
      'description' => 'Acute Illness Medication Distribution',
      'fields' => [
        'prescribed_medication' => ['amoxicillin', 'paracetamol'],
        'non_administration_reason' => ['none'],
        'type' => 'medication_distribution',
      ],
    ],
    'travel_history' => [
      'description' => 'Acute Illness Travel History',
      'fields' => [
        'travel_history' => ['covid19-country'],
        'type' => 'travel_history',
      ],
    ],
    'treatment_history' => [
      'description' => 'Acute Illness Treatment Review',
      'fields' => [
        'treatment_history' => ['fever-past-six-hours', 'malaria-today'],
        'type' => 'treatment_history',
      ],
    ],
    'exposure' => [
      'description' => 'Acute Illness Exposure',
      'fields' => [
        'exposure' => ['covid19-symptoms'],
        'type' => 'exposure',
      ],
    ],
    'isolation' => [
      'description' => 'Acute Illness Isolation',
      'fields' => [
        'isolation' => ['patient-isolated', 'health-education'],
        'reason_for_not_isolating' => ['n-a'],
        'type' => 'isolation',
      ],
    ],
    'hc_contact' => [
      'description' => 'Acute Illness HC Contact',
      'fields' => [
        'hc_contact' => ['contact-hc'],
        'hc_recommendation' => ['come-to-hc'],
        'hc_response_time' => ['less-than-30m'],
        'ambulance_arrival_time' => ['n-a'],
        'type' => 'hc_contact',
      ],
    ],
    'call_114' => [
      'description' => 'Acute Illness Call 114',
      'fields' => [
        '114_contact' => ['call-114'],
        '114_recommendation' => ['send-to-hc'],
        'site_recommendation' => ['send-with-form'],
        'type' => 'call_114',
      ],
    ],
    'acute_illness_muac' => [
      'description' => 'Acute Illness MUAC',
      'fields' => [
        'muac' => 12.5,
        'type' => 'acute_illness_muac',
      ],
    ],
    'treatment_ongoing' => [
      'description' => 'Acute Illness Treatment Ongoing',
      'fields' => [
        'treatment_ongoing' => ['taken-as-prescribed'],
        'reason_for_not_taking' => 'none',
        'missed_doses' => 0,
        'adverse_events' => ['none'],
        'type' => 'treatment_ongoing',
      ],
    ],
    'acute_illness_core_exam' => [
      'description' => 'Acute Illness Core Exam',
      'fields' => [
        'heart' => ['normal-rate-and-rhythm'],
        'lungs' => ['wheezes'],
        'type' => 'acute_illness_core_exam',
      ],
    ],
    'acute_illness_danger_signs' => [
      'description' => 'Acute Illness Danger Signs',
      'fields' => [
        'acute_illness_danger_signs' => ['vomiting', 'respiratory-distress'],
        'type' => 'acute_illness_danger_signs',
      ],
    ],
    'acute_illness_nutrition' => [
      'description' => 'Acute Illness Nutrition',
      'fields' => [
        'nutrition_signs' => ['edema'],
        'type' => 'acute_illness_nutrition',
      ],
    ],
    'health_education' => [
      'description' => 'Acute Illness Health Education',
      'fields' => [
        'health_education_signs' => ['education-for-diagnosis'],
        'reason_not_given_education' => 'none',
        'type' => 'health_education',
      ],
    ],
    'acute_illness_follow_up' => [
      'description' => 'Acute Illness Follow Up',
      'fields' => [
        'follow_up_options' => ['1-d'],
        'acute_illness_diagnosis' => 'malaria-complicated',
        'date_concluded' => $date,
        'type' => 'acute_illness_follow_up',
      ],
    ],
    'acute_illness_contacts_tracing' => [
      'description' => 'Acute Illness Contacts Tracing',
      'fields' => [
        'contacts_trace_data' => [$person_uuid . '[&]John[&]Doe[&]male[&]0781234567[&]' . $date],
        'type' => 'acute_illness_contacts_tracing',
      ],
    ],
    'acute_illness_trace_contact' => [
      'description' => 'Acute Illness Trace Contact',
      'fields' => [
        'referred_person' => $person_uuid,
        'first_name' => 'John',
        'second_name' => 'Doe',
        'gender' => 'male',
        'phone_number' => '0781234567',
        'contact_date' => $date,
        'date_concluded' => $date,
        'last_follow_up_date' => $date,
        'symptoms_general' => ['fever'],
        'symptoms_respiratory' => ['cough'],
        'symptoms_gi' => ['nausea'],
        'trace_outcome' => 'no-symptoms',
        'type' => 'acute_illness_trace_contact',
      ],
    ],
  ];

  foreach ($acute_illness_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($acute_illness_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // HOME VISIT ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $home_visit_participant_uuid = generate_uuid();
  $test_cases['individual_participant_home_visit'] = [
    'description' => 'IndividualEncounterParticipant for Home Visit',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'home-visit',
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
      'uuid' => $home_visit_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $home_visit_encounter_uuid = generate_uuid();
  $test_cases['home_visit_encounter'] = [
    'description' => 'HomeVisitEncounter',
    'entity' => [
      'individual_participant' => $home_visit_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'deleted' => FALSE,
      'type' => 'home_visit_encounter',
      'uuid' => $home_visit_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $home_visit_base = [
    'person' => $person_uuid,
    'home_visit_encounter' => $home_visit_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $home_visit_measurements = [
    'nutrition_feeding' => [
      'description' => 'Home Visit Nutrition Feeding',
      'fields' => [
        'nutrition_feeding_signs' => ['receive-supplement', 'breastfeeding'],
        'supplement_type' => 'rutf',
        'sachets_per_day' => 3.0,
        'type' => 'nutrition_feeding',
      ],
    ],
    'nutrition_hygiene' => [
      'description' => 'Home Visit Nutrition Hygiene',
      'fields' => [
        'nutrition_hygiene_signs' => ['soap-in-the-house', 'wash-hands-before-feeding'],
        'main_water_source' => 'piped-water-to-home',
        'water_preparation_option' => 'boiled',
        'type' => 'nutrition_hygiene',
      ],
    ],
    'nutrition_food_security' => [
      'description' => 'Home Visit Nutrition Food Security',
      'fields' => [
        'food_security_signs' => ['household-got-food'],
        'main_income_source' => 'home-based-agriculture',
        'type' => 'nutrition_food_security',
      ],
    ],
    'nutrition_caring' => [
      'description' => 'Home Visit Nutrition Caring',
      'fields' => [
        'nutrition_caring_signs' => ['parent-alive-and-healthy', 'child-clean'],
        'child_caring_options' => 'parent',
        'type' => 'nutrition_caring',
      ],
    ],
  ];

  foreach ($home_visit_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($home_visit_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // WELL CHILD ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $well_child_participant_uuid = generate_uuid();
  $test_cases['individual_participant_well_child'] = [
    'description' => 'IndividualEncounterParticipant for Well Child',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'well-child',
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
      'uuid' => $well_child_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $well_child_encounter_uuid = generate_uuid();
  $test_cases['well_child_encounter'] = [
    'description' => 'WellChildEncounter',
    'entity' => [
      'individual_participant' => $well_child_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'well_child_encounter_type' => 'pediatric-care',
      'encounter_notes' => ['none'],
      'encounter_warnings' => ['none'],
      'deleted' => FALSE,
      'type' => 'well_child_encounter',
      'uuid' => $well_child_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $well_child_base = [
    'person' => $person_uuid,
    'well_child_encounter' => $well_child_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $well_child_measurements = [
    'well_child_height' => [
      'description' => 'Well Child Height',
      'fields' => [
        'height' => 85.5,
        'type' => 'well_child_height',
      ],
    ],
    'well_child_muac' => [
      'description' => 'Well Child MUAC',
      'fields' => [
        'muac' => 14.0,
        'type' => 'well_child_muac',
      ],
    ],
    'well_child_weight' => [
      'description' => 'Well Child Weight',
      'fields' => [
        'weight' => 12.5,
        'type' => 'well_child_weight',
      ],
    ],
    'well_child_photo' => [
      'description' => 'Well Child Photo',
      'fields' => [
        'photo' => 'https://example.com/well_child_photo.jpg',
        'type' => 'well_child_photo',
      ],
    ],
    'well_child_nutrition' => [
      'description' => 'Well Child Nutrition',
      'fields' => [
        'nutrition_signs' => ['edema'],
        'nutrition_assesment' => ['assesment-underweight-moderate'],
        'type' => 'well_child_nutrition',
      ],
    ],
    'well_child_send_to_hc' => [
      'description' => 'Well Child Send To HC',
      'fields' => [
        'send_to_hc' => ['refer-to-hc'],
        'reason_not_sent_to_hc' => 'none',
        'type' => 'well_child_send_to_hc',
      ],
    ],
    'well_child_contributing_factors' => [
      'description' => 'Well Child Contributing Factors',
      'fields' => [
        'contributing_factors_signs' => ['lack-of-breast-milk'],
        'type' => 'well_child_contributing_factors',
      ],
    ],
    'well_child_follow_up' => [
      'description' => 'Well Child Follow Up',
      'fields' => [
        'follow_up_options' => ['1-w'],
        'nutrition_assesment' => ['assesment-underweight-moderate'],
        'nutrition_signs' => ['edema'],
        'date_concluded' => $date,
        'type' => 'well_child_follow_up',
      ],
    ],
    'well_child_health_education' => [
      'description' => 'Well Child Health Education',
      'fields' => [
        'health_education_signs' => ['education-for-diagnosis'],
        'reason_not_given_education' => 'none',
        'type' => 'well_child_health_education',
      ],
    ],
    'well_child_symptoms_review' => [
      'description' => 'Well Child Symptoms Review',
      'fields' => [
        'well_child_symptoms' => ['breathing-problems', 'diarrhea'],
        'type' => 'well_child_symptoms_review',
      ],
    ],
    'well_child_vitals' => [
      'description' => 'Well Child Vitals',
      'fields' => [
        'sys' => 90.0,
        'dia' => 60.0,
        'heart_rate' => 100,
        'respiratory_rate' => 24,
        'body_temperature' => 37.0,
        'type' => 'well_child_vitals',
      ],
    ],
    'well_child_ecd' => [
      'description' => 'Well Child ECD',
      'fields' => [
        'ecd_signs' => ['follow-mothers-eyes', 'smile', 'hold-head-without-support'],
        'type' => 'well_child_ecd',
      ],
    ],
    'well_child_head_circumference' => [
      'description' => 'Well Child Head Circumference',
      'fields' => [
        'head_circumference' => 45.0,
        'measurement_notes' => ['none'],
        'type' => 'well_child_head_circumference',
      ],
    ],
    'well_child_albendazole' => [
      'description' => 'Well Child Albendazole',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'well_child_albendazole',
      ],
    ],
    'well_child_mebendezole' => [
      'description' => 'Well Child Mebendezole',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'well_child_mebendezole',
      ],
    ],
    'well_child_vitamin_a' => [
      'description' => 'Well Child Vitamin A',
      'fields' => [
        'administration_note' => 'administered-today',
        'type' => 'well_child_vitamin_a',
      ],
    ],
    'well_child_pregnancy_summary' => [
      'description' => 'Well Child Pregnancy Summary',
      'fields' => [
        'expected_date_concluded' => $date,
        'delivery_complications' => ['gestational-diabetes'],
        'pregnancy_summary_signs' => ['apgar-scores', 'birth-length'],
        'birth_defects' => ['none'],
        'apgar_one_min' => 8.0,
        'apgar_five_min' => 9.0,
        'weight' => 3200.0,
        'height' => 50.0,
        'type' => 'well_child_pregnancy_summary',
      ],
    ],
    'well_child_next_visit' => [
      'description' => 'Well Child Next Visit',
      'fields' => [
        'immunisation_date' => $date,
        'pediatric_visit_date' => $date,
        'date_concluded' => $date,
        'asap_immunisation_date' => $date,
        'type' => 'well_child_next_visit',
      ],
    ],
    'well_child_bcg_immunisation' => [
      'description' => 'Well Child BCG Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_bcg_immunisation',
      ],
    ],
    'well_child_dtp_immunisation' => [
      'description' => 'Well Child DTP Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_dtp_immunisation',
      ],
    ],
    'well_child_dtp_sa_immunisation' => [
      'description' => 'Well Child DTP Standalone Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_dtp_sa_immunisation',
      ],
    ],
    'well_child_hpv_immunisation' => [
      'description' => 'Well Child HPV Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_hpv_immunisation',
      ],
    ],
    'well_child_ipv_immunisation' => [
      'description' => 'Well Child IPV Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_ipv_immunisation',
      ],
    ],
    'well_child_mr_immunisation' => [
      'description' => 'Well Child MR Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_mr_immunisation',
      ],
    ],
    'well_child_opv_immunisation' => [
      'description' => 'Well Child OPV Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_opv_immunisation',
      ],
    ],
    'well_child_pcv13_immunisation' => [
      'description' => 'Well Child PCV13 Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_pcv13_immunisation',
      ],
    ],
    'well_child_rotarix_immunisation' => [
      'description' => 'Well Child Rotarix Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'well_child_rotarix_immunisation',
      ],
    ],
    'well_child_ncda' => [
      'description' => 'Well Child NCDA',
      'fields' => [
        'ncda_signs' => ['receive-vitamin-a'],
        'anc_visits_dates' => [$date],
        'receive_option' => 'yes',
        'stunting_level' => 'green',
        'birth_weight' => 3200.0,
        'weight' => 12.5,
        'muac' => 14.0,
        'type' => 'well_child_ncda',
      ],
    ],
    'well_child_feeding' => [
      'description' => 'Well Child Feeding',
      'fields' => [
        'nutrition_feeding_signs' => ['breastfeeding'],
        'supplement_type' => 'none',
        'sachets_per_day' => 0.0,
        'type' => 'well_child_feeding',
      ],
    ],
    'well_child_hygiene' => [
      'description' => 'Well Child Hygiene',
      'fields' => [
        'nutrition_hygiene_signs' => ['soap-in-the-house'],
        'main_water_source' => 'piped-water-to-home',
        'water_preparation_option' => 'boiled',
        'type' => 'well_child_hygiene',
      ],
    ],
    'well_child_caring' => [
      'description' => 'Well Child Caring',
      'fields' => [
        'nutrition_caring_signs' => ['parent-alive-and-healthy'],
        'child_caring_options' => 'parent',
        'type' => 'well_child_caring',
      ],
    ],
    'well_child_food_security' => [
      'description' => 'Well Child Food Security',
      'fields' => [
        'food_security_signs' => ['household-got-food'],
        'main_income_source' => 'home-based-agriculture',
        'type' => 'well_child_food_security',
      ],
    ],
  ];

  foreach ($well_child_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($well_child_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // NCD ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $ncd_participant_uuid = generate_uuid();
  $test_cases['individual_participant_ncd'] = [
    'description' => 'IndividualEncounterParticipant for NCD',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'ncd',
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
      'uuid' => $ncd_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $ncd_encounter_uuid = generate_uuid();
  $test_cases['ncd_encounter'] = [
    'description' => 'NCDEncounter',
    'entity' => [
      'individual_participant' => $ncd_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'ncd_diagnoses' => ['diagnosis-hypertension-stage1'],
      'deleted' => FALSE,
      'type' => 'ncd_encounter',
      'uuid' => $ncd_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $ncd_base = [
    'person' => $person_uuid,
    'ncd_encounter' => $ncd_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $ncd_measurements = [
    'ncd_co_morbidities' => [
      'description' => 'NCD Co-Morbidities',
      'fields' => [
        'comorbidities' => ['diabetes', 'hypertension'],
        'type' => 'ncd_co_morbidities',
      ],
    ],
    'ncd_core_exam' => [
      'description' => 'NCD Core Exam',
      'fields' => [
        'head_hair' => ['normal'],
        'eyes' => ['pale-conjuctiva'],
        'heart' => ['normal-rate-and-rhythm'],
        'heart_murmur' => FALSE,
        'neck' => ['normal'],
        'lungs' => ['normal'],
        'abdomen' => ['normal'],
        'hands' => ['normal'],
        'legs' => ['edema'],
        'type' => 'ncd_core_exam',
      ],
    ],
    'ncd_creatinine_test' => [
      'description' => 'NCD Creatinine Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'creatinine_result' => 1.2,
        'bun_result' => 15.0,
        'type' => 'ncd_creatinine_test',
      ],
    ],
    'ncd_danger_signs' => [
      'description' => 'NCD Danger Signs',
      'fields' => [
        'ncd_danger_signs' => ['none'],
        'type' => 'ncd_danger_signs',
      ],
    ],
    'ncd_family_history' => [
      'description' => 'NCD Family History',
      'fields' => [
        'ncd_family_history_signs' => ['hypertension-history', 'diabetes-history'],
        'hypertension_predecessors' => ['father'],
        'heart_problem_predecessors' => ['none'],
        'diabetes_predecessors' => ['mother'],
        'type' => 'ncd_family_history',
      ],
    ],
    'ncd_family_planning' => [
      'description' => 'NCD Family Planning',
      'fields' => [
        'family_planning_signs' => ['pill'],
        'type' => 'ncd_family_planning',
      ],
    ],
    'ncd_health_education' => [
      'description' => 'NCD Health Education',
      'fields' => [
        'ncd_health_education_signs' => ['hypertension'],
        'type' => 'ncd_health_education',
      ],
    ],
    'ncd_hiv_test' => [
      'description' => 'NCD HIV Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['none'],
        'test_result' => 'negative',
        'hiv_signs' => ['none'],
        'type' => 'ncd_hiv_test',
      ],
    ],
    'ncd_labs_results' => [
      'description' => 'NCD Labs Results',
      'fields' => [
        'performed_tests' => ['creatinine'],
        'completed_tests' => ['creatinine'],
        'date_concluded' => $date,
        'patient_notified' => TRUE,
        'type' => 'ncd_labs_results',
      ],
    ],
    'ncd_liver_function_test' => [
      'description' => 'NCD Liver Function Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'alt_result' => 25.0,
        'ast_result' => 30.0,
        'type' => 'ncd_liver_function_test',
      ],
    ],
    'ncd_medication_distribution' => [
      'description' => 'NCD Medication Distribution',
      'fields' => [
        'recommended_treatment' => ['treatment-hypertension-add-hctz'],
        'ncd_guidance' => ['return-1m'],
        'type' => 'ncd_medication_distribution',
      ],
    ],
    'ncd_medication_history' => [
      'description' => 'NCD Medication History',
      'fields' => [
        'causing_hypertension' => ['none'],
        'treating_hypertension' => ['ace-inhibitors'],
        'treating_diabetes' => ['metformin'],
        'type' => 'ncd_medication_history',
      ],
    ],
    'ncd_outside_care' => [
      'description' => 'NCD Outside Care',
      'fields' => [
        'outside_care_signs' => ['seen-at-health-center'],
        'medical_conditions' => ['diabetes'],
        'outside_care_medications' => ['medication-metformin'],
        'type' => 'ncd_outside_care',
      ],
    ],
    'ncd_pregnancy_test' => [
      'description' => 'NCD Pregnancy Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_result' => 'negative',
        'type' => 'ncd_pregnancy_test',
      ],
    ],
    'ncd_random_blood_sugar_test' => [
      'description' => 'NCD Random Blood Sugar Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'test_prerequisites' => ['fasting-12h'],
        'sugar_count' => 110.0,
        'type' => 'ncd_random_blood_sugar_test',
      ],
    ],
    'ncd_referral' => [
      'description' => 'NCD Referral',
      'fields' => [
        'referrals' => ['hospital'],
        'reasons_for_non_referrals' => ['none'],
        'type' => 'ncd_referral',
      ],
    ],
    'ncd_social_history' => [
      'description' => 'NCD Social History',
      'fields' => [
        'ncd_social_history_signs' => ['alcohol-use', 'smoking'],
        'food_group' => 'vegetables-and-fruit',
        'beverages_per_week' => 5,
        'cigarettes_per_week' => 10,
        'type' => 'ncd_social_history',
      ],
    ],
    'ncd_symptom_review' => [
      'description' => 'NCD Symptom Review',
      'fields' => [
        'ncd_group1_symptoms' => ['dizziness'],
        'ncd_group2_symptoms' => ['numbness-or-tingling'],
        'ncd_pain_symptoms' => ['headache'],
        'type' => 'ncd_symptom_review',
      ],
    ],
    'ncd_urine_dipstick_test' => [
      'description' => 'NCD Urine Dipstick Test',
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
        'type' => 'ncd_urine_dipstick_test',
      ],
    ],
    'ncd_vitals' => [
      'description' => 'NCD Vitals',
      'fields' => [
        'sys' => 140.0,
        'dia' => 90.0,
        'sys_repeated' => 138.0,
        'dia_repeated' => 88.0,
        'heart_rate' => 78,
        'respiratory_rate' => 16,
        'body_temperature' => 36.8,
        'type' => 'ncd_vitals',
      ],
    ],
    'ncd_lipid_panel_test' => [
      'description' => 'NCD Lipid Panel Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'unit_of_measurement' => 'mmol-l',
        'total_cholesterol' => 5.2,
        'ldl_cholesterol' => 3.0,
        'hdl_cholesterol' => 1.4,
        'triglycerides' => 1.5,
        'type' => 'ncd_lipid_panel_test',
      ],
    ],
    'ncd_hba1c_test' => [
      'description' => 'NCD HbA1c Test',
      'fields' => [
        'test_execution_note' => 'run-today',
        'execution_date' => $date,
        'hba1c_result' => 6.5,
        'type' => 'ncd_hba1c_test',
      ],
    ],
  ];

  foreach ($ncd_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($ncd_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // CHILD SCOREBOARD ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $child_scoreboard_participant_uuid = generate_uuid();
  $test_cases['individual_participant_child_scoreboard'] = [
    'description' => 'IndividualEncounterParticipant for Child Scoreboard',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'child-scoreboard',
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
      'uuid' => $child_scoreboard_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $child_scoreboard_encounter_uuid = generate_uuid();
  $test_cases['child_scoreboard_encounter'] = [
    'description' => 'ChildScoreboardEncounter',
    'entity' => [
      'individual_participant' => $child_scoreboard_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'deleted' => FALSE,
      'type' => 'child_scoreboard_encounter',
      'uuid' => $child_scoreboard_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $child_scoreboard_base = [
    'person' => $person_uuid,
    'child_scoreboard_encounter' => $child_scoreboard_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $child_scoreboard_measurements = [
    'child_scoreboard_bcg_iz' => [
      'description' => 'Child Scoreboard BCG Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_bcg_iz',
      ],
    ],
    'child_scoreboard_dtp_iz' => [
      'description' => 'Child Scoreboard DTP Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_dtp_iz',
      ],
    ],
    'child_scoreboard_dtp_sa_iz' => [
      'description' => 'Child Scoreboard DTP Standalone Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_dtp_sa_iz',
      ],
    ],
    'child_scoreboard_ipv_iz' => [
      'description' => 'Child Scoreboard IPV Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_ipv_iz',
      ],
    ],
    'child_scoreboard_mr_iz' => [
      'description' => 'Child Scoreboard MR Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_mr_iz',
      ],
    ],
    'child_scoreboard_opv_iz' => [
      'description' => 'Child Scoreboard OPV Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_opv_iz',
      ],
    ],
    'child_scoreboard_pcv13_iz' => [
      'description' => 'Child Scoreboard PCV13 Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_pcv13_iz',
      ],
    ],
    'child_scoreboard_rotarix_iz' => [
      'description' => 'Child Scoreboard Rotarix Immunisation',
      'fields' => [
        'administered_doses' => ['dose-1'],
        'administration_dates' => [$date],
        'administration_note' => 'administered-today',
        'type' => 'child_scoreboard_rotarix_iz',
      ],
    ],
    'child_scoreboard_ncda' => [
      'description' => 'Child Scoreboard NCDA',
      'fields' => [
        'ncda_signs' => ['receive-vitamin-a'],
        'anc_visits_dates' => [$date],
        'receive_option' => 'yes',
        'stunting_level' => 'green',
        'birth_weight' => 3200.0,
        'weight' => 10.0,
        'muac' => 13.5,
        'type' => 'child_scoreboard_ncda',
      ],
    ],
  ];

  foreach ($child_scoreboard_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($child_scoreboard_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // TUBERCULOSIS ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $tuberculosis_participant_uuid = generate_uuid();
  $test_cases['individual_participant_tuberculosis'] = [
    'description' => 'IndividualEncounterParticipant for Tuberculosis',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'tuberculosis',
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
      'uuid' => $tuberculosis_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $tuberculosis_encounter_uuid = generate_uuid();
  $test_cases['tuberculosis_encounter'] = [
    'description' => 'TuberculosisEncounter',
    'entity' => [
      'individual_participant' => $tuberculosis_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'deleted' => FALSE,
      'type' => 'tuberculosis_encounter',
      'uuid' => $tuberculosis_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $tuberculosis_base = [
    'person' => $person_uuid,
    'tuberculosis_encounter' => $tuberculosis_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $tuberculosis_measurements = [
    'tuberculosis_diagnostics' => [
      'description' => 'Tuberculosis Diagnostics',
      'fields' => [
        'tuberculosis_diagnosis' => 'pulmonary-tb',
        'type' => 'tuberculosis_diagnostics',
      ],
    ],
    'tuberculosis_dot' => [
      'description' => 'Tuberculosis DOT',
      'fields' => [
        'dot_signs' => 'directly-observed-treatment',
        'dot_meds_distribution_sign' => 'meds-distribution-sign-yes',
        'type' => 'tuberculosis_dot',
      ],
    ],
    'tuberculosis_follow_up' => [
      'description' => 'Tuberculosis Follow Up',
      'fields' => [
        'follow_up_options' => ['1-w'],
        'date_concluded' => $date,
        'type' => 'tuberculosis_follow_up',
      ],
    ],
    'tuberculosis_health_education' => [
      'description' => 'Tuberculosis Health Education',
      'fields' => [
        'tb_health_education_signs' => ['education-follow-up-testing'],
        'type' => 'tuberculosis_health_education',
      ],
    ],
    'tuberculosis_medication' => [
      'description' => 'Tuberculosis Medication',
      'fields' => [
        'prescribed_tb_medications' => ['rifampicin', 'isoniazid'],
        'type' => 'tuberculosis_medication',
      ],
    ],
    'tuberculosis_referral' => [
      'description' => 'Tuberculosis Referral',
      'fields' => [
        'send_to_hc' => ['refer-to-hc'],
        'reason_not_sent_to_hc' => 'none',
        'type' => 'tuberculosis_referral',
      ],
    ],
    'tuberculosis_symptom_review' => [
      'description' => 'Tuberculosis Symptom Review',
      'fields' => [
        'tuberculosis_symptoms' => ['night-sweats', 'blood-in-sputum', 'weight-loss'],
        'type' => 'tuberculosis_symptom_review',
      ],
    ],
    'tuberculosis_treatment_review' => [
      'description' => 'Tuberculosis Treatment Review',
      'fields' => [
        'treatment_ongoing' => ['taken-as-prescribed'],
        'reason_for_not_taking' => 'none',
        'missed_doses' => 0,
        'adverse_events' => ['none'],
        'type' => 'tuberculosis_treatment_review',
      ],
    ],
  ];

  foreach ($tuberculosis_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($tuberculosis_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  // =========================================================================
  // HIV ENCOUNTER AND MEASUREMENTS
  // =========================================================================

  $hiv_participant_uuid = generate_uuid();
  $test_cases['individual_participant_hiv'] = [
    'description' => 'IndividualEncounterParticipant for HIV',
    'entity' => [
      'person' => $person_uuid,
      'encounter_type' => 'hiv',
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
      'uuid' => $hiv_participant_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $hiv_encounter_uuid = generate_uuid();
  $test_cases['hiv_encounter'] = [
    'description' => 'HIVEncounter',
    'entity' => [
      'individual_participant' => $hiv_participant_uuid,
      'scheduled_date' => [
        'value' => $date,
        'value2' => NULL,
      ],
      'deleted' => FALSE,
      'type' => 'hiv_encounter',
      'uuid' => $hiv_encounter_uuid,
      'status' => 1,
      'shard' => $health_center_uuid,
    ],
  ];

  $hiv_base = [
    'person' => $person_uuid,
    'hiv_encounter' => $hiv_encounter_uuid,
    'date_measured' => $date,
    'nurse' => $nurse_uuid,
    'health_center' => $health_center_uuid,
    'status' => 1,
    'shard' => $health_center_uuid,
  ];

  $hiv_measurements = [
    'hiv_diagnostics' => [
      'description' => 'HIV Diagnostics',
      'fields' => [
        'hiv_diagnosis_signs' => ['positive-result-previously'],
        'positive_result_date' => $date,
        'test_result' => 'positive',
        'type' => 'hiv_diagnostics',
      ],
    ],
    'hiv_follow_up' => [
      'description' => 'HIV Follow Up',
      'fields' => [
        'follow_up_options' => ['1-m'],
        'date_concluded' => $date,
        'type' => 'hiv_follow_up',
      ],
    ],
    'hiv_health_education' => [
      'description' => 'HIV Health Education',
      'fields' => [
        'hiv_health_education_signs' => ['education-positive-result', 'education-safe-sex'],
        'type' => 'hiv_health_education',
      ],
    ],
    'hiv_medication' => [
      'description' => 'HIV Medication',
      'fields' => [
        'prescribed_hiv_medications' => ['dolutegravir', 'tenofovir', 'lamivudine'],
        'type' => 'hiv_medication',
      ],
    ],
    'hiv_referral' => [
      'description' => 'HIV Referral',
      'fields' => [
        'send_to_hc' => ['refer-to-hc'],
        'reason_not_sent_to_hc' => 'none',
        'type' => 'hiv_referral',
      ],
    ],
    'hiv_symptom_review' => [
      'description' => 'HIV Symptom Review',
      'fields' => [
        'hiv_symptoms' => ['weight-loss', 'fatigue'],
        'type' => 'hiv_symptom_review',
      ],
    ],
    'hiv_treatment_review' => [
      'description' => 'HIV Treatment Review',
      'fields' => [
        'treatment_ongoing' => ['taken-as-prescribed'],
        'reason_for_not_taking' => 'none',
        'missed_doses' => 0,
        'adverse_events' => ['rash-itching'],
        'type' => 'hiv_treatment_review',
      ],
    ],
  ];

  foreach ($hiv_measurements as $key => $measurement) {
    $test_cases[$key] = [
      'description' => $measurement['description'],
      'entity' => array_merge($hiv_base, $measurement['fields'], [
        'deleted' => FALSE,
        'uuid' => generate_uuid(),
      ]),
    ];
  }

  return $test_cases;
}