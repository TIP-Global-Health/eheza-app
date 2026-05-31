<?php

/**
 * @file
 * Contains HedleyRestfulNutritionHeights.
 */

/**
 * Class HedleyRestfulNutritionHeights.
 */
class HedleyRestfulNutritionHeights extends HedleyRestfulNutritionActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_height',
    'field_zscore_age',
  ];

}
