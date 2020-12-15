<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionPhoto.
 */

/**
 * Class HedleyMigrateNutritionPhoto.
 */
class HedleyMigrateNutritionPhoto extends HedleyMigrateNutritionMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'nutrition_photo';

  /**
   * HedleyMigrateNutritionPhoto constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->addFieldMapping('field_photo', 'field_photo');
    $this->addFieldMapping('field_photo:file_replace')
      ->defaultValue(FILE_EXISTS_REPLACE);
    $this->addFieldMapping('field_photo:source_dir')
      ->defaultValue($this->getMigrateDirectory() . '/images/');
  }

}
