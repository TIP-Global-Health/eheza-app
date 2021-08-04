<?php

/**
 * @file
 * Contains HedleyRestfulFollowUp.
 */

/**
 * Class HedleyRestfulFollowUp.
 */
class HedleyRestfulFollowUp extends HedleyRestfulGroupActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_follow_up_options',
    'field_nutrition_assesment',
    'field_nutrition_signs',
  ];

}
