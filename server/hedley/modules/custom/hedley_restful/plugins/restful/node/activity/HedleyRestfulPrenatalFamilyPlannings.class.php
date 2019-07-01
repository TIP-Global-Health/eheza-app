<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalFamilyPlannings.
 */

/**
 * Class HedleyRestfulPrenatalFamilyPlannings.
 */
class HedleyRestfulPrenatalFamilyPlannings extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_family_planning_signs',
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
