<?php

/**
 * @file
 * Contains \HedleyMigrateSymptomsGI.
 */

/**
 * Class HedleyMigrateSymptomsGI.
 */
class HedleyMigrateSymptomsGI extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'symptoms_gi';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_bloody_diarrhea_period',
        'field_non_bloody_diarrhea_period',
        'field_nausea_period',
        'field_vomiting_period',
        'field_abdominal_pain_period',
        'field_symptoms_gi_derived_signs',
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
        'field_bloody_diarrhea_period',
        'field_non_bloody_diarrhea_period',
        'field_nausea_period',
        'field_vomiting_period',
        'field_abdominal_pain_period',
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
        'field_symptoms_gi_derived_signs',
      ]
    );
  }

}
