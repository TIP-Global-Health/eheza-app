<?php

/**
 * @file
 * Contains HedleyRestfulCounselingTopics.
 */

/**
 * Class HedleyRestfulCounselingTopics.
 */
class HedleyRestfulCounselingTopics extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    // The english is in the "label" field.
    $public_fields['kinyarwanda_title'] = [
      'property' => 'field_kinyarwanda_title',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_kinyarwanda_title',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }
  }

}
