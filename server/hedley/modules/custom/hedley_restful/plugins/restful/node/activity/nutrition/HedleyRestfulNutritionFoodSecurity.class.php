<?php

/**
 * @file
 * Contains HedleyRestfulNutritionFoodSecurity.
 */

/**
 * Class HedleyRestfulNutritionFoodSecurity.
 */
class HedleyRestfulNutritionFoodSecurity extends HedleyRestfulHomeVisitActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_main_income_source',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_food_security_signs',
  ];

}
