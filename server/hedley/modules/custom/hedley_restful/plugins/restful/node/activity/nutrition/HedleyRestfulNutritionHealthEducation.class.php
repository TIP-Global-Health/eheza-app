<?php

/**
 * @file
 * Contains HedleyRestfulNutritionHealthEducation.
 */

/**
 * Class HedleyRestfulNutritionHealthEducation.
 */
class HedleyRestfulNutritionHealthEducation extends HedleyRestfulNutritionActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_health_education_signs',
  ];


  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_reason_not_given_education',
  ];

}
