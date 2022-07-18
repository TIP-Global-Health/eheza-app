<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalSendToHC.
 */

/**
 * Class HedleyRestfulPrenatalSendToHC.
 */
class HedleyRestfulPrenatalSendToHC extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_reason_not_sent_to_hc',
    'field_referral_facility',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_send_to_hc',
  ];

}
