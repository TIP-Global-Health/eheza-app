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
   * Create a height entity.
   *
   * @param int $session_id
   *   The node ID of the session.
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
  public function createHeight($session_id, $child_id, $height, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'height']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_height->set($height);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a weight entity.
   *
   * @param int $session_id
   *   The node ID of the session.
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
  public function createWeight($session_id, $child_id, $weight, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'weight']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_person->set($child_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_weight->set($weight);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create an MUAC entity.
   *
   * @param int $session_id
   *   The node ID of the session.
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
  public function createNutrition($session_id, $child_id, $nutrition_sign, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode([
      'type' => 'nutrition',
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
    $wrapper->field_session->set($session_id);
    $wrapper->save();

    return $node->nid;
  }

}
