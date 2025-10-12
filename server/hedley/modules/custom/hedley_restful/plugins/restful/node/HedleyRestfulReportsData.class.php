<?php

/**
 * @file
 * Contains \HedleyRestfulReportsData.
 */

use Ramsey\Uuid\Uuid;

/**
 * Class HedleyRestfulReportsData.
 */
class HedleyRestfulReportsData extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Data batch limit.
   */
  const HEDLEY_RESTFUL_QUERY_BATCH = 5000;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'getData',
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
   *
   * @return array
   *   A representation of the required data.
   *
   * @throws \RestfulBadRequestException
   */
  public function getData() {
    $request = $this->getRequest();

    $app_type = $request['app_type'];
    if (empty($app_type)) {
      throw new RestfulBadRequestException('Must provide the application type parameter.');
    }

    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($request['base_revision'])) {
      // Must specify the last revision which the client already knows about.
      // This can be 0 if the client knows nothing.
      throw new RestfulBadRequestException('Must provide base_revision, indicating the last revision the client already has.');
    }
    $base = $request['base_revision'];

    $province = !empty($request['province']) ? $request['province'] : NULL;
    $district = !empty($request['district']) ? $request['district'] : NULL;
    $sector = !empty($request['sector']) ? $request['sector'] : NULL;
    $cell = !empty($request['cell']) ? $request['cell'] : NULL;
    $village = !empty($request['village']) ? $request['village'] : NULL;
    $health_center = !empty($request['health_center']) ? $request['health_center'] : NULL;

    switch ($app_type) {
      case 'reports':
        return hedley_reports_generate_results_data_for_restful($base, self::HEDLEY_RESTFUL_QUERY_BATCH, $province, $district, $sector, $cell, $village, $health_center);

      case 'ncda':
        return hedley_ncda_generate_results_data_for_restful($base, self::HEDLEY_RESTFUL_QUERY_BATCH, $province, $district, $sector, $cell, $village);

      default:
        return [];
    }
  }

}
