<?php

/**
 * @file
 * Contains HedleyRestfulWellChildFollowUp.
 */

/**
 * Class HedleyRestfulWellChildFollowUp.
 */
class HedleyRestfulWellChildFollowUp extends HedleyRestfulWellChildActivityBase {

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
    'field_follow_up_options',
    'field_nutrition_assesment',
    'field_nutrition_signs',
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_date_concluded',
  ];

}
