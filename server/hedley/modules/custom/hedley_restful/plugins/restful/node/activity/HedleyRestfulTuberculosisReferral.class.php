<?php

/**
 * @file
 * Contains HedleyRestfulTuberculosisReferral.
 */

/**
 * Class HedleyRestfulTuberculosisReferral.
 */
class HedleyRestfulTuberculosisReferral extends HedleyRestfulTuberculosisActivityBase {

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
