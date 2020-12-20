<?php

/**
 * @file
 * Contains \HedleyRestfulReportState.
 */

use Ramsey\Uuid\Uuid;

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
   * Process and record the report.
   *
   * @return array
   */
  public function processReport() {
    $request = $this->getRequest();
    $account = $this->getAccount();

    $wrapper = entity_metadata_wrapper('user', $account->uid);
    $wrapper->field_version->set($request['version']);
    $wrapper->field_total_to_upload->set($request['total_to_upload']);
    $wrapper->save();

    return [];
  }

}
