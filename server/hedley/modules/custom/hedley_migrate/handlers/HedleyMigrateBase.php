<?php

/**
 * @file
 * Contains \HedleyMigrateBase.
 */

/**
 * Class HedleyMigrateBase.
 */
abstract class HedleyMigrateBase extends Migration {

  /**
   * The entity type.
   *
   * @var string
   */
  protected $entityType = NULL;

  /**
   * The bundle.
   *
   * @var string
   */
  protected $bundle = NULL;

  /**
   * Additional columns not in the CSV.
   *
   * @var array
   */
  protected $columns = [];

  /**
   * The fields.
   *
   * @var array
   */
  protected $fields = [];

  /**
   * A sub-directory where we can find the CSV for this migration.
   *
   * @var string
   */
  protected $csvPrefix = '';

  /**
   * The name of our key field.
   *
   * @var string
   */
  protected $keyName = 'id';

  /**
   * Returns list of columns in CSV file.
   *
   * @return array
   *   List of columns in CSV file.
   */
  protected function csvColumns() {
    return [];
  }

  /**
   * Returns list of fields that we can map in a standard way.
   *
   * @return array
   *   List of fields that we can map in a standard way.
   */
  protected function simpleMappings() {
    return [];
  }

  /**
   * Returns list of fields with multi-values that we can map in a standard way.
   *
   * @return array
   *   List of fields with multi-values that we can map in a standard way.
   */
  protected function simpleMultipleMappings() {
    return [];
  }

  /**
   * HedleyMigrateBase constructor.
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $destination_classes = [
      'node' => 'MigrateDestinationNode',
      'taxonomy_term' => 'MigrateDestinationTerm',
    ];

    // Add default settings, only for nodes, terms and multifields.
    if (empty($destination_classes[$this->entityType])) {
      return;
    }

    $this->description = "Import {$this->bundle}";

    $source_file = $this->getMigrateDirectory() . '/csv/' . $this->csvPrefix . $this->bundle . '.csv';

    $columnsCsv = $this->csvColumns();
    foreach ($columnsCsv as $column_name) {
      $this->columns[] = [$column_name, $column_name];
    }
    $this->source = new MigrateSourceCSV($source_file, $this->columns, ['header_rows' => 1], $this->fields);

    $destination_class = $destination_classes[$this->entityType];
    $this->destination = new $destination_class($this->bundle);

    $key = [
      $this->keyName => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    ];

    $this->map = new MigrateSQLMap($this->machineName, $key, $this->destination->getKeySchema($this->entityType));

    // Add simple mappings.
    $mappingsSimple = $this->simpleMappings();
    if (!empty($mappingsSimple)) {
      $this->addSimpleMappings(drupal_map_assoc($mappingsSimple));
    }
    foreach ($this->simpleMultipleMappings() as $field) {
      $this
        ->addFieldMapping($field, $field)
        ->separator('|');
    }

    if ($this->entityType == 'node') {
      // Set the first user as the author.
      $this
        ->addFieldMapping('uid', 'author')
        ->defaultValue(1);
      // Map the title field to the default title.
      if (in_array('title', $columnsCsv)) {
        $this->addFieldMapping('title', 'title');
      }
      elseif (in_array('title_field', $columnsCsv)) {
        $this->addFieldMapping('title', 'title_field');
      }

      if (in_array('created', $columnsCsv)) {
        $this->addFieldMapping('created', 'created');
      }
    }
    elseif ($this->entityType == 'taxonomy_term') {
      // Map the translated name field to the default term name.
      if (in_array('name', $columnsCsv)) {
        $this->addFieldMapping('name', 'name');
      }
    }
  }

  /**
   * Returns the migrate directory.
   *
   * @return string
   *   The migrate directory.
   */
  protected function getMigrateDirectory() {
    return variable_get('hedley_migrate_directory', FALSE) ? variable_get('hedley_migrate_directory') : drupal_get_path('module', 'hedley_migrate');
  }

  /**
   * Add a JPG extensions to the file name.
   *
   * @param string $name
   *   A user name.
   *
   * @return string
   *   A file name.
   */
  public function avatarProcess($name) {
    return $name . '.jpg';
  }

  /**
   * Convert a date string to a timestamp.
   *
   * @param string $date
   *   A string containing a date.
   *
   * @return int
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

    if (preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $trimmed)) {
      return DateTime::createFromFormat('!Y-m-d', $trimmed, new DateTimeZone("UTC"))->getTimestamp();
    }

    throw new Exception("$date was not a recognized date format.");
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
  public function date2Process($date) {
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

    $timezone = new DateTimeZone("UTC");
    $value1 = DateTime::createFromFormat('!Y-m-d', $values[0], $timezone)->getTimestamp();

    if ($count == 1) {
      return [
        'value' => $value1,
        'value2' => NULL,
      ];
    }

    if (!preg_match('/^\\d\\d\\d\\d-\\d\\d-\\d\\d$/', $values[1])) {
      throw new Exception("$values[1] was not a recognized date format.");
    }

    $value2 = DateTime::createFromFormat('!Y-m-d', $values[1], $timezone)->getTimestamp();

    return [
      'value' => $value1,
      'arguments' => ['to' => $value2],
    ];
  }

}
