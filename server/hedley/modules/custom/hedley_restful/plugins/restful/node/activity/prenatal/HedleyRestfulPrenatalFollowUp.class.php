<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalFollowUp.
 */

/**
 * Class HedleyRestfulPrenatalFollowUp.
 */
class HedleyRestfulPrenatalFollowUp extends HedleyRestfulPrenatalActivityBase {


  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_prenatal_assesment',
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
