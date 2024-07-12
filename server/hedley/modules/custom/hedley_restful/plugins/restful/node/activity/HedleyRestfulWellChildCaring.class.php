<?php

/**
 * @file
 * Contains HedleyRestfulWellChildCaring.
 */

/**
 * Class HedleyRestfulWellChildCaring.
 */
class HedleyRestfulWellChildCaring extends HedleyRestfulWellChildActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_nutrition_caring_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_child_caring_options',
  ];

}
