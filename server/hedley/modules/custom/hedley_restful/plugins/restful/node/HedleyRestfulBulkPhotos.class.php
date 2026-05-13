<?php

/**
 * @file
 * Contains \HedleyRestfulBulkPhotos.
 */

/**
 * Class HedleyRestfulBulkPhotos.
 *
 * Bulk photo fetch endpoint. Accepts a JSON list of styled-photo URLs and
 * returns a custom binary container so the client can populate its photo
 * cache in one HTTP request instead of one per photo.
 */
class HedleyRestfulBulkPhotos extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Hard cap on URLs accepted per request.
   */
  const MAX_BATCH = 200;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'bulkFetch',
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
   * Stub: returns 501 until implemented.
   */
  public function bulkFetch() {
    throw new \RestfulServerConfigurationException('Not implemented yet.');
  }

}
