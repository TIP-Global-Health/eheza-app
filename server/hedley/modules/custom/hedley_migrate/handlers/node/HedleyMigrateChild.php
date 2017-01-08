<?php

/**
 * @file
 * Contains \HedleyMigrateChild.
 */

/**
 * Class HedleyMigrateChild.
 */
class HedleyMigrateChild extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'child';

  /**
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import Children from the CSV.');
    $this->dependencies = [
      'HedleyMigrateUsers',
    ];

    $column_names = [
      'title',
    ];

    $columns = [];
    foreach ($column_names as $column_name) {
      $columns[] = [$column_name, $column_name];
    }

    $source_file = $this->getMigrateDirectory() . '/csv/child.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'title' => array(
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ),
    );

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $simple_fields = drupal_map_assoc([
      'title',
    ]);

    $this->addSimpleMappings($simple_fields);

    // Map the file name to the title.
    $this->addFieldMapping('field_avatar', 'title')
      ->callbacks([$this, 'avatarProcess']);

    $this->addFieldMapping('field_avatar:file_replace')
      ->defaultValue(FILE_EXISTS_REPLACE);

    $this->addFieldMapping('field_avatar:source_dir')
      ->defaultValue($this->getMigrateDirectory() . '/images/');

    $this->addFieldMapping('uid', 'author')
      ->sourceMigration('HedleyMigrateUsers')
      ->defaultValue(1);
  }

}
