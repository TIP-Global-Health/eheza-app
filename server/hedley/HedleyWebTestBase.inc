<?php

/**
 * @file
 * A WebTestBase for Hedley profile.
 */

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
      'hedley_restful',
    ]);

    call_user_func_array(['HedleyWebTestBase', 'parent::setUp'], $modules);

    // Known issue: https://www.drupal.org/node/1217340
    features_revert();

    // If pusher keys are not set, they will crash the application.
    variable_set('hedley_pusher_app_key', 'key');
    variable_set('hedley_pusher_app_secret', 'secret');
    variable_set('hedley_pusher_app_id', 'id');
    variable_set('hedley_pusher_app_cluster', 'cluster');
  }

  /**
   * Get pusher messages of a certain type from the pusher queue.
   *
   * @param int $sale_nid
   *   Sale node ID, for the channel name.
   * @param bool $privileged
   *   Whether to fetch messages from the privileged, or the public channels.
   * @param string $name
   *   The pusher message name.
   *
   * @return array
   *   Array of pusher messages.
   */
  protected static function getPusherMessages($sale_nid, $privileged, $name) {
    $channel = hedley_pusher_get_channel_name($sale_nid, $privileged);
    // Get the entire pusher queue, and filter it by message name and channel.
    $queue = &drupal_static('hedley_pusher_static_queue', []);
    return array_filter($queue, function ($message) use ($channel, $name) {
      return $message['name'] == $name && $message['channel'] == $channel;
    });
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
   * Create a health center.
   *
   * @return int
   *   The node ID.
   */
  public function createHealthCenter() {
    $node = $this->drupalCreateNode([
      'type' => 'health_center',
    ]);

    return $node->nid;
  }

  /**
   * Create a clinic and assign it ot health center.
   *
   * @param int $health_center_id
   *   The health center to assign the clinic to.
   *
   * @return int
   *   The node ID.
   */
  public function createClinic($health_center_id) {
    $node = $this->drupalCreateNode([
      'type' => 'clinic',
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_health_center->set($health_center_id);
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
      'title' => $this->randomName(),
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_birth_date->set(strtotime('1998-07-01'));
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_ubudehe->set(1);
    $wrapper->field_education_level->set(2);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a child.
   *
   * @param int $mother_id
   *   The mother's node ID.
   *
   * @return int
   *   The child's node ID.
   */
  public function createChild($mother_id) {
    $node = $this->drupalCreateNode([
      'type' => 'person',
      'title' => $this->randomName(),
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_birth_date->set(strtotime('2015-06-27'));
    $wrapper->field_gender->set('male');
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
  function createRelationship ($person_id, $related_to, $related_by) {
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

}
