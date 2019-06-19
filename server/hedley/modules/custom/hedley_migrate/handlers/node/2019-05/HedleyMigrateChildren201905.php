<?php

/**
 * @file
 * Contains \HedleyMigrateChildren201905.
 */

/**
 * Class HedleyMigrateChildren201905.
 */
class HedleyMigrateChildren201905 extends HedleyMigrateBase {

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
  public $csvPrefix = '2019-05/';

  /**
   * {@inheritdoc}
   */
  public $columns = [
    0 => ['id', 'id'],
    1 => ['mother_id', 'mother_id'],
    2 => ['first_name', 'first_name'],
    3 => ['middle_name', 'middle_name'],
    4 => ['second_name', 'second_name'],
    5 => ['birth_date', 'birth_date'],
    8 => ['gender', 'gender'],
    22 => ['health_center', 'health_center'],
  ];

  /**
   * {@inheritdoc}
   */
  public $fields = [
    'title' => 'title',
  ];

  /**
   * HedleyMigrateChildren201905 constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->dependencies = [
      'HedleyMigrateMothers_2019_05',
    ];

    $this->addFieldMapping('title', 'title');
    $this->addFieldMapping('field_gender', 'gender');

    $this
      ->addFieldMapping('field_date_birth', 'birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_mother', 'mother_id')
      ->sourceMigration('HedleyMigrateMothers_2019_05');
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
        throw new Exception("{$row->gender} is not a recognized gender for {$row->id}");
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

    $matches = [];

    if (preg_match('/^(\\d\\d?)-(\\d\\d?)-(\\d\\d?)$/', $trimmed, $matches)) {
      // This is sometimes y-m-d and sometimes m-d-y, so we need to figure
      // that out.
      if (intval($matches[2]) > 12) {
        return DateTime::createFromFormat('!m-d-y', $trimmed)->getTimestamp();
      }

      if (intval($matches[1]) > 12) {
        return DateTime::createFromFormat('!y-m-d', $trimmed)->getTimestamp();
      }

      // The first part is 12 or less, so if the last part is 17 or 18 then
      // we're probably m-d-y.
      if (($matches[3] === '17') || ($matches[3] === '18')) {
        return DateTime::createFromFormat('!m-d-y', $trimmed)->getTimestamp();
      }
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
