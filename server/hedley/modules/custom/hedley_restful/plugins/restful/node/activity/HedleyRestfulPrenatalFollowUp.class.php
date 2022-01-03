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
    'field_contact_date',
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
    'field_contact_date',
  ];

}
