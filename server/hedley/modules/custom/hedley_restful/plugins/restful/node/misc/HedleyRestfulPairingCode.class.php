<?php

/**
 * @file
 * Contains HedleyRestfulPairingCode.
 *
 * Adapted from RestfulRefreshTokenAuthentication.
 */

/**
 * Class HedleyRestfulPairingCode.
 *
 * Conceptually, the pairing code for a device is exactly analogous to a refresh
 * token. You can use it to get an access token, instead of providing a username
 * and password.
 *
 * The only differences between a pairing code and a refresh token are:
 *
 * - the pairing code is a simple 8-digit number, so it's easier for an admin
 *   to read it out over the phone to a nurse who needs it (for example) to pair
 *   or re-pair a device.
 *
 * - the pairing code is more easily visible on the `device` entity (so the
 *   admin who is logged into the backend can easily look it up).
 *
 * So, the code below is adapted from RestfulRefreshTokenAuthentication. The
 * intended flow is something like this:
 *
 * - Nurse has new device to setup, or device that has had its cache cleared.
 *
 * - Nurse calls admin to get pairing code.
 *
 * - Admin looks up pairing code for appropriate device on backend (or creates
 *   new device on backend).
 *
 * - Admin gives pairing code to nurse.
 *
 * - Nurse enters pairing code into client app.
 *
 * - Client app contacts /api/pairing-code with pairing code (like you would
 *   contact /refresh-token with a refresh token).
 *
 * - If we verify the pairing code, we respond like /refresh-token responds,
 *   with the access token.
 *
 * Then, the client can use the access token in the usual way -- it
 * authenticates the device as the robot user we create for each device.
 */
class HedleyRestfulPairingCode extends \RestfulTokenAuthenticationBase {

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return array(
      '.*' => array(
        // Get or create a new token.
        \RestfulInterface::GET => 'pairingCode',
      ),
    );
  }

  /**
   * Create an access token for a device robot, and return its value.
   *
   * @param string $code
   *   The pairing code.
   *
   * @throws RestfulBadRequestException
   *
   * @return \RestfulTokenAuth
   *   The new access token.
   */
  public function pairingCode($code) {
    // Check if there is a published device with this code.
    $query = hedley_general_create_entity_field_query_excluding_deleted();
    $results = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'device')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_pairing_code', 'value', $code)
      ->range(0, 1)
      ->execute();

    if (empty($results['node'])) {
      throw new \RestfulBadRequestException('Invalid pairing code.');
    }

    // Get the device with the specified pairing code. The uid of the
    // device is its robot user.
    $device = node_load(key($results['node']));
    $uid = $device->uid;

    if (empty($uid)) {
      throw new \RestfulBadRequestException('Device had invalid robot user.');
    }

    // The pairing code is one-shot, so we'll delete it now that we've used it.
    // An admin can enter a new pairing code on the backend if necessary,
    // when 'super_user' mode is activated.
    $wrapper = entity_metadata_wrapper('node', $device);
    $wrapper->field_pairing_code->set('');
    $wrapper->save();

    // Delete existing access tokens and refresh tokens for the robot. We're
    // about to create new ones!
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', $this->getEntityType())
      ->propertyCondition('uid', $uid)
      ->execute();

    if (!empty($result['restful_token_auth'])) {
      $ids = array_keys($result['restful_token_auth']);
      entity_delete_multiple('restful_token_auth', $ids);
    }

    // Create the new tokens and return them.
    $controller = entity_get_controller($this->getEntityType());
    $token = $controller->generateAccessToken($uid);
    $output = $this->viewEntity($token->id);

    // As we have already calculated, add the device node ID here instead of in
    // `self::publicFieldsInfo`.
    $output['device_id'] = $device->nid;

    return $output;
  }

}
