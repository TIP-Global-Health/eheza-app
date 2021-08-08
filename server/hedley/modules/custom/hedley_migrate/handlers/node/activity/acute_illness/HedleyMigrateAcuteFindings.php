<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteFindings.
 */

/**
 * Class HedleyMigrateAcuteFindings.
 */
class HedleyMigrateAcuteFindings extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_findings';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_findings_signs_general',
        'field_findings_signs_respiratory',
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
        'field_findings_signs_general',
        'field_findings_signs_respiratory',
      ]
    );
  }

}
