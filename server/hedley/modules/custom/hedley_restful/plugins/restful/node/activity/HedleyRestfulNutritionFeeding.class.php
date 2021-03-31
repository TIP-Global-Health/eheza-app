<?php

/**
 * @file
 * Contains HedleyRestfulNutritionFeeding.
 */

/**
 * Class HedleyRestfulNutritionFeeding.
 */
class HedleyRestfulNutritionFeeding extends HedleyRestfulHomeVisitActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_supplement_type'
    'field_sachets_per_day',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_feeding_signs',
  ];

}
