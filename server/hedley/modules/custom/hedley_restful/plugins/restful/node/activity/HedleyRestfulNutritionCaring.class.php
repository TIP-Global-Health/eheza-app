<?php

/**
 * @file
 * Contains HedleyRestfulNutritionCaring.
 */

/**
 * Class HedleyRestfulNutritionCaring.
 */
class HedleyRestfulNutritionCaring extends HedleyRestfulHomeVisitActivityBase {
  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_caring_questions',
  ];

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_child_caring_options',
  ];

}
