<?php

/**
 * @file
 * Contains \HedleyMigrateSymptomsGeneral.
 */

/**
 * Class HedleyMigrateSymptomsGeneral.
 */
class HedleyMigrateSymptomsGeneral extends HedleyMigrateAcuteIllnessMeasurementBase {

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'symptoms_general';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_fever_period',
        'field_chills_period',
        'field_night_sweats_period',
        'field_body_aches_period',
        'field_headache_period',
        'field_lethargy_period',
        'field_poor_suck_period',
        'field_unable_to_drink_period',
        'field_unable_to_eat_period',
        'field_increased_thirst_period',
        'field_dry_mouth_period',
        'field_severe_weakness_period',
        'field_yellow_eyes_period',
        'field_coke_colored_urine_period',
        'field_convulsions_period',
        'field_spontaneos_bleeding_period',
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
        'field_fever_period',
        'field_chills_period',
        'field_night_sweats_period',
        'field_body_aches_period',
        'field_headache_period',
        'field_lethargy_period',
        'field_poor_suck_period',
        'field_unable_to_drink_period',
        'field_unable_to_eat_period',
        'field_increased_thirst_period',
        'field_dry_mouth_period',
        'field_severe_weakness_period',
        'field_yellow_eyes_period',
        'field_coke_colored_urine_period',
        'field_convulsions_period',
        'field_spontaneos_bleeding_period',
      ]
    );
  }

}
