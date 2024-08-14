<?php

/**
 * @file
 * Contains HedleyRestfulChildScoreboardActivityBase.
 */

/**
 * Class HedleyRestfulChildScoreboardActivityBase.
 */
abstract class HedleyRestfulChildScoreboardActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['child_scoreboard_encounter'] = [
      'property' => 'field_child_scoreboard_encounter',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_child_scoreboard_encounter', FALSE);
    // Get the UUID of the Prenatal encounter.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_child_scoreboard_encounter.field_child_scoreboard_encounter_target_id", 'uuid_child_scoreboard_encounter');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->child_scoreboard_encounter = $item->uuid_child_scoreboard_encounter;
      unset($item->uuid_child_scoreboard_encounter);
    }

    return $items;
  }

}
