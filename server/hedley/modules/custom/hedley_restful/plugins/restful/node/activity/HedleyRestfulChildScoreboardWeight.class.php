<?php

/**
 * @file
 * Contains HedleyRestfulChildScoreboardWeight.
 */

/**
 * Class HedleyRestfulChildScoreboardWeight.
 */
class HedleyRestfulChildScoreboardWeight extends HedleyRestfulChildScoreboardActivityBase {

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
