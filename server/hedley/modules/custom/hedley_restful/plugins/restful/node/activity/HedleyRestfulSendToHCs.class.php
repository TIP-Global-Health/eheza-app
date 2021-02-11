<?php

/**
 * @file
 * Contains HedleyRestfulSendToHCs.
 */

/**
 * Class HedleyRestfulSendToHCs.
 */
class HedleyRestfulSendToHCs extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_send_to_hc',
  ];

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_not_sending_to_hc',
  ];

}
