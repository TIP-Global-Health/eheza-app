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
    user_multiple_role_edit([$account->uid], "add role", $role->rid);

    $wrapper = entity_metadata_wrapper('user', $account);
    $wrapper->field_clinics->set([$clinic_id]);
    $wrapper->save();

    return $account;
  }

  /**
   * Create a clinic with a random name.
   *
   * @return int
   *   The node ID.
   */
  public function createClinic() {
    return $this->drupalCreateNode([
      'type' => 'clinic',
    ]);
  }

  /**
   * Create a mother.
   *
   * @param string $name
   *   The mother's name.
   * @param string $clinic_id
   *   The clinic to assign the mother to.
   *
   * @return int
   *   The node ID of the mother.
   */
  public function createMother($name, $clinic_id) {
    $node = $this->drupalCreateNode([
      'type' => 'mother',
      'title' => $name,
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_date_birth->set(strtotime('1998-07-01'));
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_ubudehe->set(1);
    $wrapper->field_education_level->set(2);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a child.
   *
   * @param string $name
   *   The child's name.
   * @param int $mother_id
   *   The mother's node ID.
   *
   * @return int
   *   The child's node ID.
   */
  public function createChild($name, $mother_id) {
    $node = $this->drupalCreateNode([
      'type' => 'child',
      'title' => $name,
    ]);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_mother->set($mother_id);
    $wrapper->field_date_birth->set(strtotime('2015-06-27'));
    $wrapper->field_gender->set('male');
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
   * @param bool $is_training
   *   Optional; Whether the session is a training session or not, defaults to
   *   FALSE.
   *
   * @return int
   *   The node ID of the session.
   */
  public function createSession($clinic_id, $timestamp = REQUEST_TIME, $is_training = FALSE) {
    $date = date('Y-m-d', $timestamp);

    $node = $this->drupalCreateNode(['type' => 'session']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_clinic->set($clinic_id);
    $wrapper->field_scheduled_date->set([
      'value' => $date,
      'value2' => $date,
    ]);
    $wrapper->field_closed->set(FALSE);

    if ($is_training) {
      $wrapper->field_training->set(TRUE);
    }

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
    $wrapper->field_mother->set($mother_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_family_planning_signs->set(["pill"]);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a height entity.
   *
   * @param int $child_id
   *   The node ID of the child.
   * @param int $session_id
   *   The node ID of the session.
   * @param float $height
   *   The height value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the height.
   */
  public function createHeight($child_id, $session_id, $height, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'height']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_child->set($child_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_height->set($height);
    $wrapper->save();

    return $node->nid;
  }

  /**
   * Create a weight entity.
   *
   * @param int $child_id
   *   The node ID of the child.
   * @param int $session_id
   *   The node ID of the session.
   * @param float $weight
   *   The weight value.
   * @param int $timestamp
   *   Optional; The date the measurement was taken, as a unix timestamp,
   *   defaults to the time of the request.
   *
   * @return int
   *   The node ID of the weight.
   */
  public function createWeight($child_id, $session_id, $weight, $timestamp = REQUEST_TIME) {
    $node = $this->drupalCreateNode(['type' => 'weight']);

    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_child->set($child_id);
    $wrapper->field_date_measured->set($timestamp);
    $wrapper->field_session->set($session_id);
    $wrapper->field_weight->set($weight);
    $wrapper->save();

    return $node->nid;
  }

}
