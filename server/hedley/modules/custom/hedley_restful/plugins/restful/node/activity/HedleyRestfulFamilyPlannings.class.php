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
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['family_planning_signs'] = [
      'property' => 'field_family_planning_signs',
    ];

    return $public_fields;
  }

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_family_planning_signs');
  }

}
