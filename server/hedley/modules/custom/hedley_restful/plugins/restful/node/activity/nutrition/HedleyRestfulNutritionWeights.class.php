<?php

/**
 * @file
 * Contains HedleyRestfulNutritionWeights.
 */

/**
 * Class HedleyRestfulNutritionWeights.
 */
class HedleyRestfulNutritionWeights extends HedleyRestfulNutritionActivityBase {

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
