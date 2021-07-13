<?php

/**
 * @file
 * Contains HedleyRestfulWellChildPregnancySummary.
 */

/**
 * Class HedleyRestfulWellChildPregnancySummary.
 */
class HedleyRestfulWellChildPregnancySummary extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_delivery_complications',
  ];

  protected $fields = [
    'field_apgars_one_minute',
    'field_apgars_five_minutes',
    'field_expected_date_concluded',
    'field_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_expected_date_concluded',
    'field_date_concluded'
  ];

}
