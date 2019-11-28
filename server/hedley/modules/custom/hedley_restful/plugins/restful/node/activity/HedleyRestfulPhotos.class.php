<?php

/**
 * @file
 * Contains HedleyRestfulPhotos.
 */

/**
 * Class HedleyRestfulPhotos.
 */
class HedleyRestfulPhotos extends HedleyRestfulChildActivityBase {

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    $public_fields['photo'] = [
      'property' => 'field_photo',
      'process_callbacks' => [
        [$this, 'imageProcess'],
      ],
      'image_styles' => ['patient-photo'],
    ];

    return $public_fields;
  }

  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    hedley_restful_join_field_to_query($query, 'node', 'field_photo');

    // Get to the `file`. We'll convert the `uri` to `field_photo`
    // in
    $query->innerJoin('file_managed', 'f', 'f.fid = field_photo.field_photo_fid');
    $query->addField('f', 'uri');
  }

  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$row) {
      $fid = $row->field_photo;
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

      unset($row->field_photo);
    }

    return $items;
  }

}
