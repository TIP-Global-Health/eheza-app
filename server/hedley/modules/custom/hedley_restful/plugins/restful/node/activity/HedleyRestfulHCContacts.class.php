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
    'field_hc_contact',
    'field_hc_recommendation',
    'field_site_recommendation',
  ];

}
