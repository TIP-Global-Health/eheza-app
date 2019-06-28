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

    $standard_fields_names = [
      'field_accompanied_by_partner',
      'field_partner_hiv_counseling',
      'field_mental_health_history',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    return $public_fields;
  }

}
