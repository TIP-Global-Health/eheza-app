<?php

/**
 * @file
 * Contains HedleyRestfulPeople.
 */

/**
 * Class HedleyRestfulPeople.
 */
class HedleyRestfulPeople extends HedleyRestfulSyncBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $standard_fields_names = [
      'field_birth_date_estimated',
      'field_cell',
      'field_district',
      'field_education_level',
      'field_first_name',
      'field_gender',
      'field_marital_status',
      'field_national_id_number',
      'field_phone_number',
      'field_province',
      'field_second_name',
      'field_sector',
      'field_ubudehe',
      'field_village',
      'field_hiv_status',
      'field_number_of_children',
      'field_mode_of_delivery',
      'field_hmis_number',
      'field_deleted',
    ];

    foreach ($standard_fields_names as $field_name) {
      $public_name = str_replace('field_', '', $field_name);
      $public_fields[$public_name] = [
        'property' => $field_name,
      ];
    }

    $public_fields['photo'] = [
      'property' => 'field_photo',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['person-photo'],
    ];

    $public_fields['birth_date'] = [
      'property' => 'field_birth_date',
      'process_callbacks' => [
        [$this, 'convertTimestampToYmd'],
      ],
    ];

    $public_fields['health_center'] = [
      'property' => 'field_health_center',
      'sub_property' => 'field_uuid',
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $field_names = [
      'field_birth_date_estimated',
      'field_cell',
      'field_district',
      'field_education_level',
      'field_first_name',
      'field_gender',
      'field_marital_status',
      'field_national_id_number',
      'field_phone_number',
      'field_province',
      'field_second_name',
      'field_sector',
      'field_ubudehe',
      'field_village',
      'field_hiv_status',
      'field_number_of_children',
      'field_mode_of_delivery',
      'field_hmis_number',
      'field_deleted',

      // Other fields.
      'field_photo',
      'field_birth_date',
      'field_health_center',
    ];

    foreach ($field_names as $field_name) {
      hedley_general_join_field_to_query($query, 'node', $field_name, FALSE);
    }

    // For the Photo, get to the `file`. We'll convert the `uri`
    // to `field_photo`.
    $query->leftJoin('file_managed', 'f', 'f.fid = field_photo.field_photo_fid');
    $query->addField('f', 'uri');

    // Get the UUID of the health center.
    hedley_general_join_field_to_query($query, 'node', 'field_uuid', FALSE, "field_health_center.field_health_center_target_id", 'uuid_health_center');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      $birth_date = explode(' ', $item->birth_date);
      $item->birth_date = !empty($birth_date[0]) ? $birth_date[0] : NULL;

      $item->birth_date_estimated = (bool) intval($item->birth_date_estimated);

      if (!empty($item->photo) && !empty($item->uri)) {
        $item->photo = image_style_url('person-photo', $item->uri);
      }

      unset($item->uri);

      $item->health_center = $item->uuid_health_center;
      unset($item->uuid_health_center);
    }

    return $items;
  }

}
