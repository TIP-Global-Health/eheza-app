<?php

/**
 * @file
 * Contains HedleyRestfulAcuteIllnessFollowUp.
 */

/**
 * Class HedleyRestfulAcuteIllnessFollowUp.
 */
class HedleyRestfulAcuteIllnessFollowUp extends HedleyRestfulAcuteIllnessActivityBase {

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
    'field_acute_illness_diagnosis',
  ];

}
