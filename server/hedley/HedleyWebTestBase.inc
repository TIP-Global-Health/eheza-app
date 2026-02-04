<?php

/**
 * @file
 * A WebTestBase for Hedley profile.
 */

use League\Container\Exception\NotFoundException;

/**
 * Class HedleyWebTestBase.
 */
class HedleyWebTestBase extends DrupalWebTestCase {

  /**
   * The profile name.
   *
   * @var string
   */
  protected $profile = 'hedley';

  /**
   * Overrides \DrupalWebTestCase::setUp().
   */
  public function setUp() {
    $modules = func_get_args();
    if (isset($modules[0]) && is_array($modules[0])) {
      $modules = $modules[0];
    }
    else {
      $modules = [];
    }

    $modules = array_merge($modules, [
      'admin_menu',
      'restful',
      'restful_token_auth',
      'hedley_device',
      'hedley_restful',
    ]);

    call_user_func_array(['HedleyWebTestBase', 'parent::setUp'], $modules);

    // Known issue: https://www.drupal.org/node/1217340
    features_revert();
  }

  /**
   * Create an admin user.
   *
   * @return \StdClass
   *   The user.
   */
  public function createAdministrator() {
    $account = $this->drupalCreateUser();

    $role = user_role_load_by_name('administrator');
    $account->roles[$role->rid] = $role->name;
    user_save($account);

    return $account;
  }

  /**
   * Create a nurse user, assigned to the indicated clinic.
   *
   * @param int $clinic_id
   *   The node ID of the clinic the nurse is assigned to.
   *
   * @return \StdClass
   *   The user.
   */
  public function createNurse($clinic_id) {
    $account = $this->drupalCreateUser();

    $role = user_role_load_by_name('nurse');
    $account->roles[$role->rid] = $role->name;
    user_save($account);

    $wrapper = entity_metadata_wrapper('user', $account);
    $wrapper->field_clinics->set([$clinic_id]);
    $wrapper->save();

    return $account;
  }

  /**
   * Create a health center with a random name.
   *
   * @return int
   *   The "health center" node ID.
   */
  public function createHealthCenter() {
    $node = $this->drupalCreateNode([
      'type' => 'health_center',
    ]);

    return $node->nid;
  }

  /**
   * Create a clinic and assign it to health center.
   *
   * @param int $health_center_id
   *   Optional; The health center node ID.
   * @param string $clinic_type
   *   Optional; The clinic type, options: pmtct,fbf,sorwathe
   *
   * @return int
   *   The node ID.
   */
  public function createClinic($health_center_id = NULL, $clinic_type = 'pmtct') {
    $node = $this->drupalCreateNode([
      'type' => 'clinic',
    ]);

    if (!$health_center_id) {
      return $node->nid;
    }

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_health_center->set($health_center_id);
    $wrapper->field_group_type->set($clinic_type);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a mother.
   *
   * @param int $clinic_id
   *   The clinic to assign the mother to.
   *
   * @return int
   *   The node ID of the mother.
   */
  public function createMother($clinic_id) {
    $node = $this->drupalCreateNode([
      'type' => 'person',
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_second_name->set($this->randomName());
    $wrapper->field_birth_date->set(strtotime('1998-07-01'));
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_ubudehe->set(1);
    $wrapper->field_education_level->set(2);
    $wrapper->field_gender->set('female');
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a child.
   *
   * @param int $mother_id
   *   The mother's node ID.
   * @param int $health_center_id
   *   Optional; The health center's node ID.
   * @param int $child_bd
   *   Optional; The child's BD, defaults to 1 year ago.
   * @param int $created
   *   Optional; The child's created date, defaults to now.
   *
   * @return int
   *   The child's node ID.
   */
  public function createChild($mother_id, $health_center_id = NULL, $child_bd = NULL, $created = NULL) {
    $node = $this->drupalCreateNode([
      'type' => 'person',
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_second_name->set($this->randomName());
    $wrapper->field_gender->set('male');

    if ($child_bd) {
      $wrapper->field_birth_date->set($child_bd);
    }
    else {
      $wrapper->field_birth_date->set(strtotime('2015-06-27'));
    }

    if ($created) {
      $wrapper->created->set($created);
    }

    if ($health_center_id) {
      // Need to add health center because health centers are assigned to nodes
      // according to referenced person.
      $wrapper->field_health_center->set($health_center_id);
    }
    $wrapper->save();

    $this->createRelationship($mother_id, $node->nid, HEDLEY_PERSON_RELATED_BY_PARENT_OF);

    return $node->nid;
  }

  /**
   * Create a relationship.
   *
   * See HEDLEY_PERSON_RELATED_BY... for constants.
   *
   * @param int $person_id
   *   The persons's node ID.
   * @param int $related_to
   *   The ID of the person related to
   * @param string $related_by
   *   How are the persons related?
   *
   * @return int
   *   The relationship's node ID.
   */
  function createRelationship($person_id, $related_to, $related_by) {
    $node = $this->drupalCreateNode([
      'type' => 'relationship',
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_related_to->set($related_to);
    $wrapper->field_related_by->set($related_by);
    $wrapper->save();

    return $node->nid;
  }


  /**
   * Create a session for the clinic, starting today.
   *
   * @param int $clinic_id
   *   The node ID of the clinic.
   * @param int $timestamp
   *   Optional; The date of the session, as a unix timestamp, defaults to the
   *   time of the request.
   *
   * @return int
   *   The node ID of the session.
   */
  public function createSession($clinic_id, $timestamp = REQUEST_TIME) {
    $date = date('Y-m-d', $timestamp);

    $node = $this->drupalCreateNode(['type' => 'session']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_scheduled_date->set([
      'value' => $date,
      'value2' => $date,
    ]);

    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an attendance for a session, starting today.
   *
   * @param int $health_center_id
   *   The node ID of the health center.
   * @param int $session_id
   *   The node ID of the session.
   * @param int $nurse_id
   *   The node ID of the nurse.
   * @param int $child_id
   *   The node ID of the child.
   * @param bool $attended
   *   Optional; Whether the participant attended the session or not, defaults
   *   to TRUE (Yes).
   * @param int $timestamp
   *   Optional; The date of the session, as a unix timestamp, defaults to the
   *   time of the request.
   *
   * @return int
   *   The node ID of the attendance.
   */
  public function createAttendance($health_center_id, $session_id, $nurse_id, $child_id, $attended = TRUE, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'attendance']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_shards->set([
      $health_center_id,
    ]);
    $wrapper->field_session->set($session_id);
    $wrapper->field_nurse->set($nurse_id);
    $wrapper->field_person->set($child_id);
    $wrapper->field_attended->set($attended);
    $wrapper->field_date_measured->set($timestamp);

    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an "PMTCT Participant".
   *
   * @param int $child_id
   *   The node ID of the child.
   * @param int $mother_id
   *   The node ID of the mother.
   * @param int $clinic_id
   *   The node ID of the clinic.
   * @param int $timestamp
   *   Optional; The date of the session, as a unix timestamp, defaults to the
   *   time of the request.
   *
   * @return int
   *   The node ID of the "PMTCT Participant".
   */
  public function createPMTCT($child_id, $mother_id, $clinic_id, $timestamp = REQUEST_TIME) {
    $date = date('Y-m-d', $timestamp);

    // We have to add the fields before saving because we have insert hooks that
    // are looking for those values.
    $options = [
      'type' => 'pmtct_participant',
      'field_adult' => [
        LANGUAGE_NONE => [
          ['target_id' => $mother_id],
        ],
      ],
      'field_person' => [
        LANGUAGE_NONE => [
          ['target_id' => $child_id],
        ],
      ],
      'field_clinic' => [
        LANGUAGE_NONE => [
          ['target_id' => $clinic_id],
        ],
      ],
      'field_expected' => [
        LANGUAGE_NONE => [
          [
            'value' => $date,
            'value2' => $date,
          ],
        ],
      ],
    ];

    $node = $this->drupalCreateNode($options);

    return $node->nid;
  }

  /**
   * Create a family-planning entity.
   *
   * @param int $mother_id
   *   The node ID of the mother.
   * @param int $session_id
   *   The node ID of the session.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the family-planning.
   */
  public function createFamilyPlanning($mother_id, $session_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'family_planning']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($mother_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_family_planning_signs->set(["pill"]);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an NCD danger signs entity.
   */
  public function createDangerSigns($type, $encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    switch ($type) {
      case 'ncd_danger_signs':
        $wrapper->field_ncd_encounter->set($encounter_id);
        $wrapper->field_ncd_danger_signs->set(['headache']);
        break;

      case 'danger_signs':
        $wrapper->field_prenatal_encounter->set($encounter_id);
        $wrapper->field_danger_signs->set(['bleeding']);
        break;

      default:
        throw new NotFoundException("$type not supported for danger signs creation.");
    }
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an NCD symptom review entity.
   */
  public function createNcdSymptomReview($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'ncd_symptom_review']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_ncd_encounter->set($encounter_id);
    $wrapper->field_ncd_group1_symptoms->set(['cough']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an NCD core exam entity.
   */
  public function createCoreExam($type, $encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    switch ($type) {
      case 'ncd_core_exam':
        $wrapper->field_ncd_encounter->set($encounter_id);
        break;

      case 'core_physical_exam':
        $wrapper->field_prenatal_encounter->set($encounter_id);
        break;

      default:
        throw new NotFoundException("$type not supported for core exam creation.");
    }

    $wrapper->field_head_hair->set(['normal']);
    $wrapper->field_eyes->set(['normal']);
    $wrapper->field_neck->set(['normal']);
    $wrapper->field_heart->set(['normal-rate-and-rhythm']);
    $wrapper->field_heart_murmur->set(0);
    $wrapper->field_lungs->set(['normal']);
    $wrapper->field_abdomen->set(['normal']);
    $wrapper->field_hands->set(['normal']);
    $wrapper->field_legs->set(['normal']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an NCD vitals entity.
   */
  public function createNcdVitals($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'ncd_vitals']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_ncd_encounter->set($encounter_id);
    $wrapper->field_sys->set(120);
    $wrapper->field_dia->set(80);
    $wrapper->field_heart_rate->set(70);
    $wrapper->field_respiratory_rate->set(18);
    $wrapper->field_body_temperature->set(36.6);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an NCD family planning entity.
   */
  public function createNcdFamilyPlanning($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'ncd_family_planning']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_ncd_encounter->set($encounter_id);
    $wrapper->field_family_planning_signs->set(['pill']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an acute illness symptom measurement entity of given type.
   */
  public function createAcuteIllnessSymptoms($type, $encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_acute_illness_encounter->set($encounter_id);
    switch ($type) {
      case 'symptoms_gi':
        $wrapper->field_nausea_period->set(2);
        $wrapper->field_bloody_diarrhea_period->set(2);
        $wrapper->field_vomiting_period->set(3);
        $wrapper->field_abdominal_pain_period->set(1);
        break;

      case 'symptoms_general':
        $wrapper->field_night_sweats_period->set(2);
        $wrapper->field_headache_period->set(2);
        $wrapper->field_poor_suck_period->set(2);
        $wrapper->field_unable_to_drink_period->set(2);
        $wrapper->field_unable_to_eat_period->set(2);
        break;

      case 'symptoms_respiratory':
        $wrapper->field_shortness_of_breath_period->set(3);
        $wrapper->field_nasal_congestion_period->set(3);
        $wrapper->field_blood_in_sputum_period->set(3);
        $wrapper->field_sore_throat_period->set(2);
        break;

      default:
        throw new NotFoundException("$type not supported for acute illness symptoms creation.");
    }
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an acute illness vitals entity.
   */
  public function createAcuteIllnessVitals($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'acute_illness_vitals']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_acute_illness_encounter->set($encounter_id);
    $wrapper->field_sys->set(110);
    $wrapper->field_dia->set(70);
    $wrapper->field_heart_rate->set(80);
    $wrapper->field_respiratory_rate->set(20);
    $wrapper->field_body_temperature->set(37.2);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an acute illness core exam entity.
   */
  public function createAcuteIllnessCoreExam($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'acute_illness_core_exam']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_acute_illness_encounter->set($encounter_id);
    $wrapper->field_heart->set(['normal-rate-and-rhythm']);
    $wrapper->field_lungs->set(['normal']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an acute illness findings entity.
   */
  public function createAcuteFindings($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'acute_findings']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_acute_illness_encounter->set($encounter_id);
    $wrapper->field_findings_signs_general->set(['jaundice']);
    $wrapper->field_findings_signs_respiratory->set(['stridor']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a child scoreboard immunization entity (BCG/DTP).
   */
  public function createChildScoreboardImmunization($type, $encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_child_scoreboard_encounter->set($encounter_id);
    $wrapper->field_administered_doses->set(['dose-1']);
    $wrapper->field_administration_note->set('administered-today');
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a child scoreboard NCDA measurement.
   */
  public function createChildScoreboardNcda($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'child_scoreboard_ncda']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_child_scoreboard_encounter->set($encounter_id);
    $wrapper->field_ncda_signs->set(['normal']);
    $wrapper->field_birth_weight->set(3.1);
    $wrapper->field_weight->set(10.2);
    $wrapper->field_muac->set(14.5);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a prenatal breastfeeding entity.
   */
  public function createPrenatalBreastfeeding($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'prenatal_breastfeeding']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_prenatal_encounter->set($encounter_id);
    $wrapper->field_breastfeeding_signs->set(['normal']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a breast exam entity.
   */
  public function createBreastExam($type, $encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    switch ($type) {
      case 'breast_exam':
        $wrapper->field_prenatal_encounter->set($encounter_id);
        break;

      default:
        throw new NotFoundException("$type not supported for breast exam creation.");
    }
    $wrapper->field_breast_exam_signs->set(['normal']);
    $wrapper->field_discharge_type->set(['none']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a prenatal GU exam entity.
   */
  public function createPrenatalGuExam($encounter_id, $person_id, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'prenatal_gu_exam']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_prenatal_encounter->set($encounter_id);
    $wrapper->field_gu_exam_signs->set(['normal']);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an individual participant node.
   *
   * @param int $person_id
   *   The node ID of the child.
   * @param int $date
   *   Enrollment date as unix timestamp, defaults to request time.
   * @param string $encounter_type
   *   Encounter type value, e.g. 'nutrition' or 'ncd'.
   *
   * @return object
   *   The saved participant node.
   */
  public function createIndividualParticipant($person_id, $date, $encounter_type) {
    $node = $this->drupalCreateNode([
      'type' => 'individual_participant',
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($person_id);
    $wrapper->field_encounter_type->set($encounter_type);
    $wrapper->field_expected->set([
      'value' => date('Y-m-d', $date),
      'value2' => date('Y-m-d', $date),
    ]);
    $wrapper->save();

    return $node;
  }

  /**
   * Create a height entity.
   *
   * @param string $type
   *   The type of Height entity. Options: 'height', 'nutrition_height'.
   * @param int $session_id
   *   The node ID of the session / individual nutrition encounter.
   * @param int $child_id
   *   The node ID of the child.
   * @param float $height
   *   The height value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the height.
   */
  public function createHeight($type, $session_id, $child_id, $height, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);

    switch ($type) {
      case 'height':
        $wrapper->field_session->set($session_id);
        break;

      case 'nutrition_height':
        $wrapper->field_nutrition_encounter->set($session_id);
        break;

      default:
        throw new NotFoundException("$type not supported for height creation.");
    }

    $wrapper->field_height->set($height);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a weight entity.
   *
   * @param string $type
   *   The type of Weight entity. Options: 'weight', 'nutrition_weight'.
   * @param int $session_id
   *   The node ID of the session / individual nutrition encounter.
   * @param int $child_id
   *   The node ID of the child.
   * @param float $weight
   *   The weight value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the weight.
   */
  public function createWeight($type, $session_id, $child_id, $weight, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);

    switch ($type) {
      case 'weight':
        $wrapper->field_session->set($session_id);
        break;

      case 'nutrition_weight':
        $wrapper->field_nutrition_encounter->set($session_id);
        break;

      default:
        throw new NotFoundException("$type not supported for weight creation.");
    }

    $wrapper->field_weight->set($weight);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an MUAC entity.
   *
   * @param string $type
   *   The type of MUAC entity. Options: 'muac', 'nutrition_mu
   * @param int $session_id
   *   The node ID of the session / individual nutrition encounter.
   * @param int $child_id
   *   The node ID of the child.
   * @param float $muac
   *   The MUAC value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the MUAC.
   */
  public function createMuac($type, $session_id, $child_id, $muac, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => $type]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);

    switch ($type) {
      case 'muac':
        $wrapper->field_session->set($session_id);
        break;

      case 'nutrition_muac':
        $wrapper->field_nutrition_encounter->set($session_id);
        break;

      default:
        throw new NotFoundException("$type not supported for MUAC creation.");
    }

    $wrapper->field_muac->set($muac);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a nutrition entity.
   *
   * @param int $session_id
   *   The node ID of the session.
   * @param int $child_id
   *   The node ID of the child.
   * @param string $nutrition_sign
   *   The nutrition sign value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the nutrition.
   */
  /**
   * Create a nutrition entity.
   *
   * @param string $type
   *   The type of Nutrition entity. Options: 'nutrition', 'nutrition_nutrition'.
   * @param int $session_id
   *   The node ID of the session / individual nutrition encounter.
   * @param int $child_id
   *   The node ID of the child.
   * @param string $nutrition_sign
   *   The nutrition sign value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the nutrition.
   */
  public function createNutrition($type, $session_id, $child_id, $nutrition_sign, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode([
      'type' => $type,
      // We have to add the nutrition signs here because we can't add it with
      // the wrapper.
      'field_nutrition_signs' => [
        LANGUAGE_NONE => [
          ['value' => $nutrition_sign],
        ],
      ],
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);

    switch ($type) {
      case 'nutrition':
        $wrapper->field_session->set($session_id);
        break;

      case 'nutrition_nutrition':
        $wrapper->field_nutrition_encounter->set($session_id);
        break;

      default:
        throw new NotFoundException("$type not supported for nutrition creation.");
    }

    $wrapper->save();

    return $node->nid;
  }

}
