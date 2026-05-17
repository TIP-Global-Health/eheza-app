<?php

/**
 * @file
 * Contains HedleyRestfulNCDReferral.
 */

/**
 * Class HedleyRestfulNCDReferral.
 */
class HedleyRestfulNCDReferral extends HedleyRestfulNCDActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_referrals',
    'field_reasons_for_non_referrals',
  ];

}
