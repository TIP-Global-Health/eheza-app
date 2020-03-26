<?php

/**
 * @file
 * Contains HedleyRestfulSocialHistories.
 */

/**
 * Class HedleyRestfulSocialHistories.
 */
class HedleyRestfulSocialHistories extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_partner_hiv_testing',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_social_history',
  ];

}
