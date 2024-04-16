<?php

/**
 * @file
 * Contains HedleyRestfulWellChildHygiene.
 */

/**
 * Class HedleyRestfulWellChildHygiene.
 */
class HedleyRestfulWellChildHygiene extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_main_water_source',
    'field_water_preparation_option',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_hygiene_signs',
  ];

}
