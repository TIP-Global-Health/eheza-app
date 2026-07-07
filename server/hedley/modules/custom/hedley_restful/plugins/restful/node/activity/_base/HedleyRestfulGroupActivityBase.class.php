<?php

/**
 * @file
 * Contains HedleyRestfulGroupActivityBase.
 */

/**
 * Class HedleyRestfulGroupActivityBase.
 */
abstract class HedleyRestfulGroupActivityBase extends HedleyRestfulActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['session'] = [
      'property' => 'field_session',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    $field_names = [
      'field_session',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // Get the UUID of the Session.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_session.field_session_target_id", 'uuid_session');

    return $query;
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $item->session = $item->uuid_session;
      unset($item->uuid_session);
    }

    return $items;
  }

}
