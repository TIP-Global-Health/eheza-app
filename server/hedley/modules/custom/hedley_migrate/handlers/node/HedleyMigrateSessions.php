<?php

/**
 * @file
 * Contains \HedleyMigrateSessions.
 */

/**
 * Class HedleyMigrateSessions.
 */
class HedleyMigrateSessions extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  protected $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  protected $bundle = 'session';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'field_clinic',
    'field_scheduled_date',
  ];

  /**
   * HedleyMigrateSessions constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateClinics',
    ];

    $this
      ->addFieldMapping('field_scheduled_date', 'field_scheduled_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_clinic', 'field_clinic')
      ->sourceMigration('HedleyMigrateClinics');
  }

  /**
   * Convert a date string to a timestamp.
   *
   * @param string $date
   *   A string containing a date.
   *
   * @return array
   *   A timestamp.
   *
   * @throws \Exception
   */
  public function dateProcess($date) {
    // The source material uses several date formats.
    $trimmed = trim($date);

    if (empty($trimmed)) {
      return $trimmed;
    }

    $values = explode('|', $trimmed);
    $count = count($values);

    if ($count == 0 || $count > 2) {
      throw new Exception("$trimmed is not a valid value.");
    }

    if (!preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $values[0])) {
      throw new Exception("$values[0] was not a recognized date format.");
    }

    $value1 = DateTime::createFromFormat('!Y-m-d', $values[0])->getTimestamp();

    if ($count == 1) {
      return [
        'value' => $value1,
        'value2' => $value1,
      ];
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $values[1])) {
      throw new Exception("$values[1] was not a recognized date format.");
    }

    $value2 = DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();

    return [
      'value' => $value1,
      'value2' => $value2,
    ];
  }

}
