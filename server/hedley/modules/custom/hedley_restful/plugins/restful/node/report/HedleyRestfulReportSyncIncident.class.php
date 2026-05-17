<?php

/**
 * @file
 * Contains \HedleyRestfulReportSyncIncident.
 */

/**
 * Class HedleyRestfulReportSyncIncident.
 */
class HedleyRestfulReportSyncIncident extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'processIncident',
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
   * Records sync incident data reported by clients.
   *
   * @return array
   *   We don't need to return anything, so we return an empty array.
   *
   * @throws \RestfulBadRequestException
   */
  public function processIncident() {
    $request = $this->getRequest();

    $incident_type = $request['incident_type'];
    if (empty($incident_type)) {
      throw new RestfulBadRequestException('Must provide incident_type parameter.');
    }

    $content_identifier = $request['content_identifier'];
    if (empty($content_identifier)) {
      throw new RestfulBadRequestException('Must provide content_identifier parameter.');
    }

    $account = $this->getAccount();
    hedley_restful_report_sync_incident($incident_type, $content_identifier, $account->uid);

    return [];
  }

}
