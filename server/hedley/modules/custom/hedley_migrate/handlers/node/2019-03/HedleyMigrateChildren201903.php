<?php

/**
 * @file
 * Contains \HedleyMigrateChildren201903.
 */

/**
 * Class HedleyMigrateChildren201903.
 */
class HedleyMigrateChildren201903 extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'child';

  /**
   * {@inheritdoc}
   */
  public $csvPrefix = '2019-03/';

  /**
   * {@inheritdoc}
   */
  public $columns = [
    0 => ['id', 'id'],
    2 => ['first_name', 'first_name'],
    3 => ['middle_name', 'middle_name'],
    4 => ['second_name', 'second_name'],
    5 => ['birth_date', 'birth_date'],
    8 => ['gender', 'gender'],
    12 => ['mother_national_id', 'mother_national_id'],
    20 => ['cell', 'cell'],
    22 => ['health_center', 'health_center'],
  ];

  /**
   * {@inheritdoc}
   */
  public $fields = [
    'title' => 'title',
  ];

  /**
   * HedleyMigrateChildren201903 constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateMothers_2019_03',
    ];

    $this->addFieldMapping('title', 'title');
    $this->addFieldMapping('field_gender', 'gender');

    $this
      ->addFieldMapping('field_date_birth', 'birth_date')
      ->callbacks([$this, 'dateProcess']);

    // In this batch, our unique ID for the mother in the source data is the
    // national ID.
    $this
      ->addFieldMapping('field_mother', 'mother_national_id')
      ->sourceMigration('HedleyMigrateMothers_2019_03');
  }

  /**
   * Calculate some values.
   */
  public function prepareRow($row) {
    if (parent::prepareRow($row) === FALSE) {
      return FALSE;
    }

    // Calculate the child's name.
    $row->title = implode(' ', array_filter([
      trim($row->second_name),
      trim($row->first_name),
      trim($row->middle_name),
    ]));

    // Normalize Gender.
    if ($row->gender) {
      $row->gender = trim(strtolower($row->gender));

      if ($row->gender == 'm') {
        $row->gender = 'male';
      }

      if ($row->gender == 'f') {
        $row->gender = 'female';
      }

      if ($row->gender != 'female' && $row->gender != 'male') {
        throw new Exception("{$row->gender} is not a recognized gender");
      }
    }

    return TRUE;
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

    if (preg_match('/^\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!y-m-d', $trimmed)->getTimestamp();
    }

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!Y-m-d', $trimmed)->getTimestamp();
    }

    if (preg_match('@^\\d\\d?/\\d\\d?/\\d\\d\\d\\d@', $trimmed)) {
      return DateTime::createFromFormat('!d/m/Y', $trimmed)->getTimestamp();
    }

    throw new Exception("$date was not a recognized date format.");
  }

}
