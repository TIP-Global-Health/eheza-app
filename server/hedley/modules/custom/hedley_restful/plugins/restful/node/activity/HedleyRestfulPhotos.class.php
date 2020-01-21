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
  /**
   * Add latest measurement photo as the Person's profile avatar.
   *
   * @param $view_entity
   *   The photo entity view.
   *
   * @throws \Exception
   *   If there was an error saving the person entity.
   */
  protected function copyImageToPerson($view_entity) {
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
      throw $exception;
    }
  }

}
