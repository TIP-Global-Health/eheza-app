<?php

/**
 * @file
 * Contains HedleyRestfulWellChildFeeding.
 */

/**
 * Class HedleyRestfulWellChildFeeding.
 */
class HedleyRestfulWellChildFeeding extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_supplement_type',
    'field_sachets_per_day',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_feeding_signs',
  ];

}
