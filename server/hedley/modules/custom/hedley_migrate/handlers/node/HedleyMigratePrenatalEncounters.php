<?php

/**
 * @file
 * Contains \HedleyMigratePrenatalEncounters.
 */

/**
 * Class HedleyMigratePrenatalEncounters.
 */
class HedleyMigratePrenatalEncounters extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'prenatal_encounter';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_prenatal_participant',
    'field_scheduled_date',
  ];

  /**
   * HedleyMigratePrenatalEncounters constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigratePrenatalParticipants',
    ];

    $this
      ->addFieldMapping('field_scheduled_date', 'field_scheduled_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_prenatal_participant', 'field_prenatal_participant')
      ->sourceMigration('HedleyMigratePrenatalParticipants');
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
