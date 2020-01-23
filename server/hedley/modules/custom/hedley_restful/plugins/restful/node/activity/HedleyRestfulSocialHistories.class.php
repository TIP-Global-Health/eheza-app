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
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['social_history'] = [
      'property' => 'field_social_history',
    ];

    $public_fields['partner_hiv_testing'] = [
      'property' => 'field_partner_hiv_testing',
    ];

    return $public_fields;
  }

}
