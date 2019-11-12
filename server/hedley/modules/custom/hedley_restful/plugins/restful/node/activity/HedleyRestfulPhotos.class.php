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

  public function alterQueryForViewWithDbSelect(SelectQuery $query) {
    // @todo: Have image style saved on the node, so we could db_query()
    // it.
    hedley_restful_join_field_to_query($query, 'node', 'field_photo');
  }

}
