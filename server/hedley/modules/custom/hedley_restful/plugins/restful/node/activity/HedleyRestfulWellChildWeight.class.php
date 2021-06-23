<?php

/**
 * @file
 * Contains HedleyRestfulWellChildWeight.
 */

/**
 * Class HedleyRestfulWellChildWeight.
 */
class HedleyRestfulWellChildWeight extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_weight',
    'field_bmi',
    'field_zscore_age',
    'field_zscore_bmi',
    'field_zscore_length',
  ];

}
