<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalUltrasound.
 */

/**
 * Class HedleyRestfulPrenatalUltrasound.
 */
class HedleyRestfulPrenatalUltrasound extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_edd_weeks',
    'field_edd_days',
    'field_expected_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_pregnancy_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_expected_date_concluded',
  ];

}
