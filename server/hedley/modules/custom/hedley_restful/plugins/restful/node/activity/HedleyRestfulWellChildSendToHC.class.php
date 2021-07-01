<?php

/**
 * @file
 * Contains HedleyRestfulWellChildSendToHC.
 */

/**
 * Class HedleyRestfulWellChildSendToHC.
 */
class HedleyRestfulWellChildSendToHC extends HedleyRestfulWellChildActivityBase {

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
    'field_reason_not_sent_to_hc',
  ];

}
