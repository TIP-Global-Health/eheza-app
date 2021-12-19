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
  protected $multiFields = [
    'field_follow_up_options',
    'field_nutrition_assesment',
    'field_nutrition_signs',
  ];

}
