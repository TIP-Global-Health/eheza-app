<?php

/**
 * @file
 * Contains HedleyRestfulNCDLabsResults.
 */

/**
 * Class HedleyRestfulNCDLabsResults.
 */
class HedleyRestfulNCDLabsResults extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_date_concluded',
    'field_review_state',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_performed_tests',
    'field_completed_tests',
    'field_tests_with_follow_up',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_date_concluded',
  ];

}
