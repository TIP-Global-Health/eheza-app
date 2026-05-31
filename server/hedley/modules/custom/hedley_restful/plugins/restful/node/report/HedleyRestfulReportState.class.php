<?php

/**
 * @file
 * Contains \HedleyRestfulReportState.
 */

/**
 * Class HedleyRestfulReportState.
 */
class HedleyRestfulReportState extends \RestfulBase implements \RestfulDataProviderInterface {

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

    $version = $request['version'];
    if (empty($version)) {
      throw new RestfulBadRequestException('Must provide version parameter.');
    }

    $phase = $request['phase'];
    if (empty($phase)) {
      throw new RestfulBadRequestException('Must provide phase parameter.');
    }

    $total_to_upload = $request['total_to_upload'];
    // Note that 0 is fine, so we can't use `empty`.
    if (!isset($total_to_upload)) {
      throw new RestfulBadRequestException('Must provide total_to_upload parameter.');
    }

    $synced_authorities = $request['synced_authorities'];
    if (!isset($synced_authorities)) {
      throw new RestfulBadRequestException('Must provide synced_authorities parameter.');
    }
    if (!empty($synced_authorities)) {
      $synced_authorities = hedley_restful_resolve_nids_for_uuids($synced_authorities);
      sort($synced_authorities);
    }
    $wrapper = entity_metadata_wrapper('user', $account->uid);
    $current_version = $wrapper->field_version->value();
    $current_phase = $wrapper->field_sync_phase->value();
    $current_total_to_upload = $wrapper->field_total_to_upload->value();
    $current_synced_authorities = $wrapper->field_health_centers->value();
    sort($current_synced_authorities);

    if ($current_version !== $version
      || $current_phase !== $phase
      || $current_total_to_upload !== $total_to_upload
      || $current_synced_authorities !== $synced_authorities
    ) {
      $wrapper->field_version->set($version);
      $wrapper->field_sync_phase->set($phase);
      $wrapper->field_total_to_upload->set($total_to_upload);
      $wrapper->field_health_centers->set($synced_authorities);
      $wrapper->save();
    }

    return [];
  }

}
