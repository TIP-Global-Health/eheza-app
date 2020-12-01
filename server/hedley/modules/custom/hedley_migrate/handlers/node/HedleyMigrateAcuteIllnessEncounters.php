<?php

/**
 * @file
 * Contains \HedleyMigrateAcuteIllnessEncounters.
 */

/**
 * Class HedleyMigrateAcuteIllnessEncounters.
 */
class HedleyMigrateAcuteIllnessEncounters extends HedleyMigrateIndividualEncounterBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'acute_illness_encounter';

  /**
   * {@inheritdoc}
   */
  protected function csvColumns() {
    $columns = parent::csvColumns();

    return array_merge(
      $columns, [
        'field_acute_illness_diagnosis',
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
        'field_acute_illness_diagnosis',
      ]
    );
  }

}
