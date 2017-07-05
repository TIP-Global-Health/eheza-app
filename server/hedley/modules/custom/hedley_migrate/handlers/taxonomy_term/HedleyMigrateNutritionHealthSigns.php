<?php

/**
 * @file
 * Contains \HedleyMigrateNutritionHealthSigns.
 */

/**
 * Class HedleyMigrateNutritionHealthSigns.
 */
class HedleyMigrateNutritionHealthSigns extends HedleyMigrateBase {

  protected $entityType = 'taxonomy_term';
  protected $bundle = 'nutrition_health_signs';

  protected $csvColumns = [
    'id',
  ];

  protected $simpleMappings = [
    'name',
  ];

  /**
   * {@inheritdoc}
   */
  public function __construct($arguments) {
    parent::__construct($arguments);
    $this->description = t('Import Nutrition/Health Signs from the CSV.');

    $column_names = [
      'name',
    ];

    $columns = [];
    foreach ($column_names as $column_name) {
      $columns[] = [$column_name, $column_name];
    }

    $source_file = $this->getMigrateDirectory() . '/csv/nutrition_health_signs.csv';
    $options = array('header_rows' => 1);
    $this->source = new MigrateSourceCSV($source_file, $columns, $options);

    $key = array(
      'name' => array(
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ),
    );

    $this->destination = new MigrateDestinationTerm($this->bundle);

    $this->map = new MigrateSQLMap($this->machineName, $key, MigrateDestinationNode::getKeySchema());

    $simple_fields = drupal_map_assoc([
      'name',
    ]);

    $this->addSimpleMappings($simple_fields);
  }

}
