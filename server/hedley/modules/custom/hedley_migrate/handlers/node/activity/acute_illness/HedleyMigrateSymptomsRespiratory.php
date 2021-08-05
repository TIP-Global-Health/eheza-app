<?php

/**
 * @file
 * Contains \HedleyMigrateSymptomsRespiratory.
 */

/**
 * Class HedleyMigrateSymptomsRespiratory.
 */
class HedleyMigrateSymptomsRespiratory extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'symptoms_respiratory';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_cough_period',
        'field_shortness_of_breath_period',
        'field_nasal_congestion_period',
        'field_blood_in_sputum_period',
        'field_sore_throat_period',
        'field_loss_of_smell_period',
        'field_stabbing_chest_pain_period',
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
        'field_cough_period',
        'field_shortness_of_breath_period',
        'field_nasal_congestion_period',
        'field_blood_in_sputum_period',
        'field_sore_throat_period',
        'field_loss_of_smell_period',
        'field_stabbing_chest_pain_period',
      ]
    );
  }

}
