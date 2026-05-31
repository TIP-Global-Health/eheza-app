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
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_send_to_hc',
    'field_referrals',
    'field_reasons_for_non_referrals',
  ];

}
