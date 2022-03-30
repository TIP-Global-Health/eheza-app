<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalLabsResults.
 */

/**
 * Class HedleyRestfulPrenatalLabsResults.
 */
class HedleyRestfulPrenatalLabsResults extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_date_concluded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_performed_tests',
    'field_completed_tests',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_date_concluded',
  ];

}
