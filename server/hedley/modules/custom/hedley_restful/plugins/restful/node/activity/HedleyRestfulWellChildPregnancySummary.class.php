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

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_expected_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_expected_date_concluded',
  ];

}
