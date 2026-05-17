<?php

/**
 * @file
 * Contains HedleyRestfulGroupSendToHC.
 */

/**
 * Class HedleyRestfulGroupSendToHC.
 */
class HedleyRestfulGroupSendToHC extends HedleyRestfulGroupActivityBase {

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
