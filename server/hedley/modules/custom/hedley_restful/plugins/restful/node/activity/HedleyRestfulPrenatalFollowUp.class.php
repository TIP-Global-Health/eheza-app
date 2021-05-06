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
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_follow_up_options',
  ];

}
