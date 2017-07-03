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

}
