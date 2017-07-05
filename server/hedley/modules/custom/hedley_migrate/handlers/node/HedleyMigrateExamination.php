<?php

/**
 * @file
 * Contains \HedleyMigrateExamination.
 */

/**
 * Class HedleyMigrateExamination.
 */
class HedleyMigrateExamination extends HedleyMigrateBase {

  public $entityType = 'node';
  public $bundle = 'examination';

  /**
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import Examination from the CSV.');
    $this->dependencies = [
      'HedleyMigrateGroup',
    ];

    $column_names = [
      'title',
      'field_group',
      'field_last_assessment',
    ];

    $columns = [];
    foreach ($column_names as $column_name) {
      $columns[] = [$column_name, $column_name];
    }

    $source_file = $this->getMigrateDirectory() . '/csv/examination.csv';
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
      'field_last_assessment',
    ]);

    $this->addSimpleMappings($simple_fields);

    $this->addFieldMapping('field_group', 'field_group')
      ->sourceMigration('HedleyMigrateGroup');

  }

}
