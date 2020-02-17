<?php

/**
 * @file
 * Contains HedleyRestfulFamilyPlannings.
 */

/**
 * Class HedleyRestfulFamilyPlannings.
 */
class HedleyRestfulFamilyPlannings extends HedleyRestfulActivityBase {

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

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_restful_join_field_to_query($query, 'node', 'field_family_planning_signs', FALSE);

    $query->addExpression("GROUP_CONCAT(DISTINCT field_family_planning_signs.field_family_planning_signs_value)", 'field_family_planning_signs');
    $query->groupBy('node.nid');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->family_planning_signs = explode(',', $item->family_planning_signs);
    }

    return $items;
  }

}
