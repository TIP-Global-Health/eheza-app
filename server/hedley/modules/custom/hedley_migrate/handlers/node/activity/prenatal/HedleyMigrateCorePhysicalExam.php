<?php

/**
 * @file
 * Contains \HedleyMigrateCorePhysicalExam.
 */

/**
 * Class HedleyMigrateCorePhysicalExam.
 */
class HedleyMigrateCorePhysicalExam extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'core_physical_exam';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_head_hair',
        'field_eyes',
        'field_neck',
        'field_heart',
        'field_heart_murmur',
        'field_lungs',
        'field_abdomen',
        'field_hands',
        'field_legs',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMappings() {
    $mappings = parent::simpleMappings();

    return array_merge(
      $mappings, [
        'field_heart_murmur',
      ]
    );
  }

  /**
   * {@inheritdoc}
   */
  protected function simpleMultipleMappings() {
    $mappings = parent::simpleMultipleMappings();

    return array_merge(
      $mappings, [
        'field_head_hair',
        'field_eyes',
        'field_neck',
        'field_heart',
        'field_lungs',
        'field_abdomen',
        'field_hands',
        'field_legs',
      ]
    );
  }

}
