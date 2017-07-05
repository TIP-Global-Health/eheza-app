<?php

/**
 * @file
 * Contains \HedleyMigrateExamination.
 */

/**
 * Class HedleyMigrateExamination.
 */
class HedleyMigrateGroup extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'group';

  /**
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import Groups from the CSV.');

    $column_names = [
      'id',
      'title',
    ];

    $columns = [];
    foreach ($column_names as $column_name) {
      $columns[] = [$column_name, $column_name];
    }

    $source_file = $this->getMigrateDirectory() . '/csv/group.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'id' => array(
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
  }

}
