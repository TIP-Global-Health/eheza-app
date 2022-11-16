<?php

/**
 * @file
 * Contains HedleyRestfulNutritionNCDA.
 */

/**
 * Class HedleyRestfulNutritionNCDA.
 */
class HedleyRestfulNutritionNCDA extends HedleyRestfulNutritionActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_weight',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_ncda_signs',
  ];

}
