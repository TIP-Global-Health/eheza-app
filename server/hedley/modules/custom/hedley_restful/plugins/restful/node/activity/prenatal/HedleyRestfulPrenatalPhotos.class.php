<?php

/**
 * @file
 * Contains HedleyRestfulPrenatalPhotos.
 */

/**
 * Class HedleyRestfulPrenatalPhotos.
 */
class HedleyRestfulPrenatalPhotos extends HedleyRestfulPrenatalActivityBase {

  /**
   * {@inheritdoc}
   */
  protected $fields = [
    'field_photo',
  ];

  /**
   * {@inheritdoc}
   */
  public function publicFieldsInfo() {
    $public_fields = parent::publicFieldsInfo();

    unset($public_fields['photo']);

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

    // For the Photo, get to the `file`. We'll convert the `uri`
    // to `field_photo`.
    $query->innerJoin('file_managed', 'f', 'f.fid = field_photo.field_photo_fid');
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

}
