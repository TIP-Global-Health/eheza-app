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
      'image_styles' => ['patient-photo'],
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

      // Other fields.
      'field_photo',
      'field_birth_date',
      'field_health_center',
    ];

    foreach ($field_names as $field_name) {
      hedley_restful_join_field_to_query($query, 'node', $field_name);
    }

    // For the Photo, get to the `file`. We'll convert the `uri` to `field_photo`
    // in
    $query->innerJoin('file_managed', 'f', 'f.fid = field_photo.field_photo_fid');
    $query->addField('f', 'uri');

    // Get the UUID of the health center.
    hedley_restful_join_field_to_query($query, 'node', 'field_uuid', TRUE, "field_health_center.field_health_center_target_id");

  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $fields_info = $this->getPublicFields();

    foreach ($items as &$row) {
      foreach ($fields_info as $public_name => $field_info) {
        if (strpos($field_info['property'], 'field_') !== 0) {
          continue;
        }

        if (!isset($row->{$field_info['property']})) {
          continue;
        }

        $row->{$public_name} = $row->{$field_info['property']};
        unset($row->{$field_info['property']});
      }

      $row->birth_date = $this->convertTimestampToYmd($row->birth_date);

      $fid = $row->photo;
      $uri = $row->uri;

      $image_style  = 'patient-photo';

      $value = [
        'id' => $fid,
        'self' => file_create_url($uri),
        'styles' => [
          $image_style => image_style_url($image_style  , $uri),
        ],
      ];

      $row->photo = $value;
    }

    return $items;
  }


}
