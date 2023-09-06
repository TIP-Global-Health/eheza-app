<?php

/**
 * @file
 * Contains \HedleyRestfulEntityBaseNode.
 */

/**
 * Class HedleyRestfulEntityBaseNode.
 */
abstract class HedleyRestfulEntityBaseNode extends \RestfulEntityBaseNode {

  /**
   * Overrides \RestfulEntityBaseNode::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['created'] = array(
      'property' => 'created',
      'process_callbacks' => [
        [$this, 'convertTimestampToIso8601'],
      ],
    );

    return $public_fields;
  }

  /**
   * Convert Unix timestamp to ISO8601.
   *
   * @param int $timestamp
   *   The Unix timestamp.
   *
   * @return false|string
   *   The converted timestamp.
   */
  protected function convertTimestampToIso8601($timestamp) {
    return date('c', $timestamp);
  }

  /**
   * Convert Unix timestamp to YYYY-MM-DD.
   *
   * @param int $timestamp
   *   The Unix timestamp.
   *
   * @return false|string
   *   The converted timestamp.
   */
  protected function convertTimestampToYmd($timestamp) {
    return date('Y-m-d', $timestamp);
  }

  /**
   * Process callback, Remove Drupal specific events from the image array.
   *
   * @param array $value
   *   The image array.
   *
   * @return array
   *   A cleaned image array.
   */
  protected function imageProcess(array $value) {
    if (static::isArrayNumeric($value)) {
      $return = [];
      foreach ($value as $item) {
        $return[] = $this->imageProcess($item);
      }
      return $return;
    }

    $return = [
      'id' => $value['fid'],
      'self' => file_create_url($value['uri']),
      'filemime' => $value['filemime'],
      'filesize' => $value['filesize'],
      'width' => $value['width'],
      'height' => $value['height'],
    ];

    if (!empty($value['image_styles'])) {
      $return['styles'] = $value['image_styles'];
    }

    return $return;
  }

  /**
   * Process callback, Render the counseling schedule.
   *
   * @return mixed
   *   The counseling schedule entities from the endpoint.
   */
  protected function renderCounselingSchedule() {
    $handler = restful_get_restful_handler('counseling-schedule');
    $handler->setAccount($this->getAccount());

    return $handler->get();
  }

  /**
   * Process callback, Render the participant forms.
   *
   * @return mixed
   *   The participant form entities from the endpoint.
   */
  protected function renderParticipantForms() {
    $handler = restful_get_restful_handler('participants-form');
    $account = $this->getAccount();
    $handler->setAccount($account);

    return $handler->get();
  }

}
