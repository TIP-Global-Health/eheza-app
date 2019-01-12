<?php

/**
 * @file
 * Contains HedleyRestfulDeviceToken.
 */

/**
 * Class HedleyRestfulDeviceToken.
 */
class HedleyRestfulDeviceToken extends \RestfulAuthenticationBase {

  /**
   * Extracting the token from a request by a key name, either dashed or not.
   *
   * @param array $request
   *   The current request.
   * @param string $param_name
   *   The param name to check.
   *
   * @return string
   *   The token from the request or FALSE if token isn't exists.
   */
  private  function extractTokenFromRequest(array $request, $param_name) {
    $key_name = !empty($param_name) ? $param_name : 'access_token';
    $dashed_key_name = str_replace('_', '-', $key_name);

    // Access token may be on the request, or in the headers
    // (may be a with dash instead of underscore).
    if (!empty($request['__application'][$key_name])) {
      return $request['__application'][$key_name];
    }
    elseif (!empty($request[$key_name])) {
      return $request[$key_name];
    }
    elseif (!empty($request['__application'][$dashed_key_name])) {
      return $request['__application'][$dashed_key_name];
    }
    elseif (!empty($request[$dashed_key_name])) {
      return $request[$dashed_key_name];
    }

    // Access token with that key name isn't exists.
    return FALSE;
  }

  /**
   * {@inheritdoc}
   */
  public function applies(array $request = array(), $method = \RestfulInterface::GET) {
    $options = $this->getPluginKey('options');

    return $this->extractTokenFromRequest($request, $options['param_name']);
  }

  /**
   * {@inheritdoc}
   */
  public function authenticate(array $request = array(), $method = \RestfulInterface::GET) {
    $options = $this->getPluginKey('options');
    $token = $this->extractTokenFromRequest($request, $options['param_name']);

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'device')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_access_token', 'value', $token)
      ->range(0, 1)
      ->execute();

    if (empty($result['node'])) {
      // No token exists.
      return;
    }

    // All devices authenticate as the 'sync' user.
    return user_load_by_name('sync');
  }

}
