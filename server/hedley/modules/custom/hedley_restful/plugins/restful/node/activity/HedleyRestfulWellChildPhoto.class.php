<?php

/**
 * @file
 * Contains HedleyRestfulWellChildPhoto.
 */

/**
 * Class HedleyRestfulWellChildPhoto.
 */
class HedleyRestfulWellChildPhoto extends HedleyRestfulWellChildActivityBase {

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
      'image_styles' => ['patient-photo'],
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
        $item->photo = image_style_url('patient-photo', $item->uri);
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
    $this->copyImageToPerson($view_entity);

    return $view_entity;
  }

  /**
   * {@inheritdoc}
   */
  public function patchEntity($entity_id) {
    $view_entity = parent::patchEntity($entity_id);
    $this->copyImageToPerson($view_entity);

    return $view_entity;
  }

  /**
   * Add latest measurement photo as the Person's profile avatar.
   *
   * @param array $view_entity
   *   The photo entity RESTful view.
   *
   * @throws \Exception
   *   If there was an error saving the person entity.
   */
  protected function copyImageToPerson(array $view_entity) {
    $entity_id = $view_entity[0]['id'];

    $wrapper = entity_metadata_wrapper($this->entityType, $entity_id);
    if (!$wrapper || !is_object($wrapper)) {
      return;
    }

    $person_wrapper = $wrapper->field_person;
    if (!$person_wrapper || !is_object($person_wrapper)) {
      return;
    }

    try {
      $person_wrapper->field_photo->set($wrapper->field_photo->value());
      $person_wrapper->save();
    }
    catch (Exception $exception) {
      watchdog('error', $exception->getMessage());

      throw $exception;
    }
  }

}
