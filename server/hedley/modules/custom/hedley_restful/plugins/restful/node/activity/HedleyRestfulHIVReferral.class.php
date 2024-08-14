<?php

/**
 * @file
 * Contains HedleyRestfulHIVReferral.
 */

/**
 * Class HedleyRestfulHIVReferral.
 */
class HedleyRestfulHIVReferral extends HedleyRestfulHIVActivityBase {

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
