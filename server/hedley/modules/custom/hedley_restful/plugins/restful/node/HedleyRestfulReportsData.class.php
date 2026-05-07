<?php

/**
 * @file
 * Contains \HedleyRestfulReportsData.
 */

/**
 * Class HedleyRestfulReportsData.
 */
class HedleyRestfulReportsData extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Data batch limit.
   */
  const HEDLEY_RESTFUL_QUERY_BATCH = 50000;

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
   * Generates reports data according to provided parameters.
   *
   * @return array
   *   A representation of the required data.
   *
   * @throws \RestfulBadRequestException
   * @throws \RestfulForbiddenException
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

    // Mirror the access policy of the admin pages that host the Elm app:
    // 'reports' / 'completion' use hedley_reports_menu_access (Statistical
    // Queries Manager + HC-scope filter); 'scoreboard' uses
    // hedley_ncda_aggregated_ncda_report_access (Data Manager).
    if ($app_type === 'reports' || $app_type === 'completion') {
      $scope_data = hedley_reports_resolve_scope_for_reports_by_current_user();
      if (empty($scope_data) || !isset($scope_data['scope'])) {
        throw new RestfulForbiddenException('Access denied.');
      }
      if ($scope_data['scope'] === HEDLEY_REPORTS_SCOPE_HEALTH_CENTERS) {
        if (empty($health_center) || !in_array($health_center, $scope_data['items'])) {
          throw new RestfulForbiddenException('Access denied for the requested health center.');
        }
      }
    }
    elseif ($app_type === 'scoreboard') {
      if (!hedley_ncda_aggregated_ncda_report_access()) {
        throw new RestfulForbiddenException('Access denied.');
      }
    }

    switch ($app_type) {
      case 'reports':
        return hedley_reports_generate_results_data_for_restful($base, self::HEDLEY_RESTFUL_QUERY_BATCH, $province, $district, $sector, $cell, $village, $health_center);

      case 'scoreboard':
        return hedley_ncda_generate_results_data_for_restful($base, self::HEDLEY_RESTFUL_QUERY_BATCH, $province, $district, $sector, $cell, $village);

      case 'completion':
        return hedley_reports_generate_completion_data_for_restful($base, self::HEDLEY_RESTFUL_QUERY_BATCH, $health_center);

      default:
        return [];
    }
  }

}
