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
  protected $fields = [
    'field_phase_recorded',
  ];

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_referrals',
    'field_reasons_for_non_referrals',
  ];

}
