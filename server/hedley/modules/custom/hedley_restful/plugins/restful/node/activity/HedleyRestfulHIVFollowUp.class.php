<?php

/**
 * @file
 * Contains HedleyRestfulHIVFollowUp.
 */

/**
 * Class HedleyRestfulHIVFollowUp.
 */
class HedleyRestfulHIVFollowUp extends HedleyRestfulHIVActivityBase {

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
  ];

  /**
   * {@inheritdoc}
   */
  protected $dateFields = [
    'field_date_concluded',
  ];

}
