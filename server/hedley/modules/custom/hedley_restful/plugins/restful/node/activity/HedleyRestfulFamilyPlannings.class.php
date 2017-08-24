<?php

/**
 * @file
 * Contains HedleyRestfulFamilyPlannings.
 */

/**
 * Class HedleyRestfulFamilyPlannings.
 */
class HedleyRestfulFamilyPlannings extends HedleyRestfulMotherActivityBase {

  /**
   * Return the type of the activity.
   *
   * @return string
   *   The type name.
   */
  protected static function getType() {
    return 'family-planning';
  }

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['family_planning_signs'] = [
      'property' => 'field_family_planning_signs',
    ];

    return $public_fields;
  }

}
