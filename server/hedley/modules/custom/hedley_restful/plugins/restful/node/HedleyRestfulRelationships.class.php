<?php

/**
 * @file
 * Contains HedleyRestfulRelationships.
 */

/**
 * Class HedleyRestfulRelationships.
 */
class HedleyRestfulRelationships extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'related_by',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['person'] = [
      'property' => 'field_person',
      'sub_property' => 'field_uuid',
    ];

    $public_fields['related_to'] = [
      'property' => 'field_related_to',
      'sub_property' => 'field_uuid',
    ];

    // The label is decorative only.
    unset($public_fields['label']);

    return $public_fields;
  }

}
