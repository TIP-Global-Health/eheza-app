<?php

/**
 * @file
 * Contains HedleyRestfulHCContacts.
 */

/**
 * Class HedleyRestfulHCContacts.
 */
class HedleyRestfulHCContacts extends HedleyRestfulAcuteIllnessActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $multiFields = [
    'field_hc_response_time',
    'field_ambulance_arrival_time',
    'field_hc_contact',
    'field_hc_recommendation',
  ];

}
