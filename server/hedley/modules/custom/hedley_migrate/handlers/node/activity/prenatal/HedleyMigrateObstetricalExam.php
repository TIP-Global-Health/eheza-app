<?php

/**
 * @file
 * Contains \HedleyMigrateObstetricalExam.
 */

/**
 * Class HedleyMigrateObstetricalExam.
 */
class HedleyMigrateObstetricalExam extends HedleyMigratePrenatalMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'obstetrical_exam';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_fundal_height',
        'field_fetal_presentation',
        'field_fetal_movement',
        'field_fetal_heart_rate',
        'field_c_section_scar',
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
        'field_fundal_height',
        'field_fetal_presentation',
        'field_fetal_movement',
        'field_fetal_heart_rate',
        'field_c_section_scar',
      ]
    );
  }

}
