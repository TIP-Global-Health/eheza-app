<?php

/**
 * @file
 * Contains HedleyRestfulNutritionHygiene.
 */

/**
 * Class HedleyRestfulNutritionHygiene.
 */
class HedleyRestfulNutritionHygiene extends HedleyRestfulHomeVisitActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_main_water_source',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_hygiene_signs',
  ];

}
