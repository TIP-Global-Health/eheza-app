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
   * The CSV columns.
   *
   * @var array
   */
  protected $csvColumns = [];

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
   * Fields which we can map in a standard way.
   *
   * @var array
   */
  protected $simpleMappings = [];

  /**
   * Fields with multi-values that we can map in a standard way.
   *
   * @var array
   */
  protected $simpleMultipleMappings = [];

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

    foreach ($this->csvColumns as $column_name) {
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
    if ($this->simpleMappings) {
      $this->addSimpleMappings(drupal_map_assoc($this->simpleMappings));
    }
    foreach ($this->simpleMultipleMappings as $field) {
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
      if (in_array('title', $this->csvColumns)) {
        $this->addFieldMapping('title', 'title');
      }
      elseif (in_array('title_field', $this->csvColumns)) {
        $this->addFieldMapping('title', 'title_field');
      }
    }
    elseif ($this->entityType == 'taxonomy_term') {
      // Map the translated name field to the default term name.
      if (in_array('name', $this->csvColumns)) {
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
   */
  public function dateProcess($date) {
    return strtotime($date);
  }

  /**
   * Add date fields.
   *
   * @param array $field_names
   *   The date related field names.
   */
  public function addDateFields(array $field_names) {
    foreach ($field_names as $field_name) {
      $this->addFieldMapping($field_name, $field_name)
        ->callbacks([$this, 'dateProcess']);
    }
  }

}
