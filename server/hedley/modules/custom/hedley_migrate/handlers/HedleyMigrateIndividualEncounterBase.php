<?php

/**
 * @file
 * Contains \HedleyMigrateIndividualEncounterBase.
 */

/**
 * Class HedleyMigrateIndividualEncounterBase.
 */
abstract class HedleyMigrateIndividualEncounterBase extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_individual_participant',
    'field_scheduled_date',
  ];

  /**
   * HedleyMigrateIndividualEncounterBase constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateIndividualParticipants',
    ];

    $this
      ->addFieldMapping('field_scheduled_date', 'field_scheduled_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_individual_participant', 'field_individual_participant')
      ->sourceMigration('HedleyMigrateIndividualParticipants');
  }

  /**
   * Convert a date string to a timestamp.
   *
   * @param string $date
   *   A string containing a date.
   *
   * @return int
   *   A timestamp.
   */
  public function dateProcess($date) {
    // The source material uses several date formats.
    $trimmed = trim($date);

    if (empty($trimmed)) {
      return $trimmed;
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      $stamp = DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();

      return [
        'value' => $stamp,
        'value2' => NULL,
      ];
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
