<?php

/**
 * @file
 * Contains \HedleyRestfulDevices.
 */

/**
 * Class HedleyRestfulDevices.
 */
class HedleyRestfulDevices extends HedleyRestfulEntityBaseNode {

  /**
   * Overrides \RestfulDataProviderEFQ::controllersInfo().
   */
  public static function controllersInfo() {
    // So far, we only do one thing here.
    //
    // If you post a pairing_code, we'll check it and return device
    // info if we find it.
    return [
      '' => [
        \RestfulInterface::POST => 'checkAccessCode',
      ],
    ];
  }

  /**
   * Check the pairing code provided.
   *
   * @return array
   *   A representation of the device, if the pairing code is found.
   *
   * @throws \RestfulBadRequestException
   */
  public function checkAccessCode() {
    $request = $this->getRequest();

    if (empty($request['pairing_code'])) {
      throw new RestfulBadRequestException('No pairing code was provided.');
    }

    $query = new EntityFieldQuery();
    $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'device')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_pairing_code', 'value', $request['pairing_code'])
      ->range(0, 1);

    $result = $query->execute();

    $nids = empty($result['node']) ? [] : array_keys($result['node']);

    if (empty($nids[0])) {
      throw new \RestfulForbiddenException('The provided pairing code was not found.');
    }

    $wrapper = entity_metadata_wrapper('node', $nids[0]);

    return [
      0 => [
        'id' => $wrapper->getIdentifier(),
        'access_token' => $wrapper->field_access_token->value(),
        'label' => $wrapper->label(),
      ]
    ];

  }

}
