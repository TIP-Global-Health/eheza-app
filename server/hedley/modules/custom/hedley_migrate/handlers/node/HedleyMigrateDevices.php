<?php

/**
 * @file
 * Contains \HedleyMigrateDevices.
 */

/**
 * Class HedleyMigrateDevices.
 */
class HedleyMigrateDevices extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'device';

  /**
   * HedleyMigrateDevices constructor.
   *
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import devices from the CSV.');
    $this->dependencies = [];

    $columns = [
      ['id', 'id'],
      ['title', 'title'],
    ];

    $source_file = $this->getMigrateDirectory() . '/csv/devices.csv';
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
