<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalNutritions.
 */

/**
 * Class HedleyRestfulPrenatalNutritions.
 */
class HedleyRestfulPrenatalNutritions extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_height',
    'field_weight',
    'field_muac',
  ];

}
