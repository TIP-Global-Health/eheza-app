<?php

/**
 * @file
 * Contains \HedleyRestfulReportState.
 */

/**
 * Class HedleyRestfulReportState.
 */
class HedleyRestfulReportIncidentDetails extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'processReport',
      ],
      '^.*$' => [
        // We do not allow any calls here.
      ],
    ];
  }

  /**
   * Implements \RestfulInterface::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    return [];
  }

  /**
   * Records device state data reported by clients.
   *
   * @return array
   *   We don't need to return anything, so we return an empty array.
   *
   * @throws \EntityMetadataWrapperException
   * @throws \RestfulBadRequestException
   */
  public function processReport() {
    $request = $this->getRequest();
    $account = $this->getAccount();

    $details = $request['incident_details'];
    if (empty($details)) {
      throw new RestfulBadRequestException('Must provide incident details.');
    }

    $wrapper = entity_metadata_wrapper('user', $account->uid);
    $current_details = $wrapper->field_incident_details->value();

    if ($current_details !== $details) {
      $wrapper->field_incident_details->set($details);
      $wrapper->save();
    }

    return [];
  }

}
