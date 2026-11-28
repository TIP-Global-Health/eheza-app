<?php

/**
 * @file
 * Contains HedleyRestfulPhotos.
 */

/**
 * Class HedleyRestfulPhotos.
 */
class HedleyRestfulPhotos extends HedleyRestfulGroupActivityBase {

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
      'image_styles' => ['person-photo'],
    ];

    return $public_fields;
  }

  /**
   * {@inheritdoc}
   */
  protected function alterQueryForViewWithDbSelect(SelectQuery $query) {
    $query = parent::alterQueryForViewWithDbSelect($query);

    hedley_general_join_field_to_query($query, 'node', 'field_photo', FALSE);

    // For the Photo, get to the `file`. We'll convert the `uri`
    // to `field_photo`.
    $query->leftJoin('file_managed', 'f', 'f.fid = field_photo.field_photo_fid');
    $query->addField('f', 'uri');
  }

  /**
   * {@inheritdoc}
   */
  protected function postExecuteQueryForViewWithDbSelect(array $items = []) {
    $items = parent::postExecuteQueryForViewWithDbSelect($items);

    foreach ($items as &$item) {
      if (!empty($item->photo) && !empty($item->uri)) {
        $item->photo = image_style_url('person-photo', $item->uri);
      }

      unset($item->uri);
    }

    return $items;
  }

  /**
   * {@inheritdoc}
   */
  public function createEntity() {
    $view_entity = parent::createEntity();
    hedley_restful_copy_photo_to_person($view_entity[0]['id']);

    return $view_entity;
  }

  /**
   * {@inheritdoc}
   */
  public function patchEntity($entity_id) {
    $view_entity = parent::patchEntity($entity_id);
    hedley_restful_copy_photo_to_person($view_entity[0]['id']);

    return $view_entity;
  }

}
