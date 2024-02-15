<?php

/**
 * @file
 * Contains HedleyRestfulWellChildFoodSecurity.
 */

/**
 * Class HedleyRestfulWellChildFoodSecurity.
 */
class HedleyRestfulWellChildFoodSecurity extends HedleyRestfulWellChildActivityBase {

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
