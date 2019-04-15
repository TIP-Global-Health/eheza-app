<?php

/**
 * @file
 * Contains \HedleyMigrateChildren201904.
 */

/**
 * Class HedleyMigrateChildren201904.
 */
class HedleyMigrateChildren201904 extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'child';

  /**
   * HedleyMigrateChildren201904 constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import children from the CSV.');
    $this->dependencies = [
      'HedleyMigrateMothers_2019_04',
    ];

    // For now, we're not using all columns. We'll eventually modify this and
    // do a `drush mi --update` when we have new fields in our data model to
    // fill.
    $columns = [
      0 => ['id', 'id'],
      1 => ['mother_id', 'mother_id'],
      2 => ['first_name', 'first_name'],
      3 => ['middle_name', 'middle_name'],
      4 => ['second_name', 'second_name'],
      5 => ['birth_date', 'birth_date'],
      8 => ['gender', 'gender'],
      22 => ['health_center', 'health_center'],
    ];

    // Will be added in prepareRow.
    $fields = [
      'title' => 'title',
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/2019-04/children.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options, $fields);

    $key = array(
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this
      ->addFieldMapping('uid', 'author')
      ->defaultValue(1);

    $this->addFieldMapping('title', 'title');
    $this->addFieldMapping('field_gender', 'gender');

    $this
      ->addFieldMapping('field_date_birth', 'birth_date')
      ->callbacks([$this, 'dateProcess']);

    $this
      ->addFieldMapping('field_mother', 'mother_id')
      ->sourceMigration('HedleyMigrateMothers_2019_04');
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
