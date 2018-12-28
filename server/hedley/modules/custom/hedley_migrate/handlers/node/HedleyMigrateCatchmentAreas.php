<?php

/**
 * @file
 * Contains \HedleyMigrateCatchmentAreas.
 */

/**
 * Class HedleyMigrateCatchmentAreas.
 */
class HedleyMigrateCatchmentAreas extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'catchment_area';

  /**
   * HedleyMigrateClinics constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import catchment areas from the CSV.');
    $this->dependencies = [];

    $columns = [
      ['id', t('id')],
      ['title', t('title')],
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/catchment-areas.csv';
    $options = ['header_rows' => 1];
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = [
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    ];

    $this->destination = new MigrateDestinationNode($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $this->addFieldMapping('title', 'title');
  }

}
