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
