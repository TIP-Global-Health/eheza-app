<?php

/**
 * @file
 * Contains \HedleyMigrateParticipantForms.
 */

/**
 * Class HedleyMigrateParticipantForms.
 */
class HedleyMigrateParticipantForms extends XMLMigration {

  /**
   * HedleyMigrateParticipantForms constructor.
   */
  public function __construct($arguments) {
    parent::__construct($arguments);

    $this->description = t('Migrate participant form entities.');

    $fields = array(
      'title' => t('Title'),
      'kinyarwanda_title' => t('Kinyarwanda title'),
      'body' => t('Body'),
      'kinyarwanda_body' => t('Kinyarwanda body'),
    );

    $source_file = $this->getMigrateDirectory() . '/xml/' . 'participant_form.xml';

    $item_xpath = '/forms/form';
    $item_ID_xpath = '@id';

    $items_class = new MigrateItemsXML($source_file, $item_xpath, $item_ID_xpath);
    $this->source = new MigrateSourceMultiItems($items_class, $fields);

    $options = [
      'text_format' => 'filtered_html',
    ];

    $this->destination = new MigrateDestinationNode('participant_form', $options);

    $key = [
      'id' => [
        'type' => 'varchar',
        'length' => 255,
        'not null' => TRUE,
      ],
    ];

    $this->map = new MigrateSQLMap(
      $this->machineName,
      $key,
      MigrateDestinationNode::getKeySchema()
    );

    $this
      ->addFieldMapping('uid', 'author')
      ->defaultValue(1);

    $this
      ->addFieldMapping('title', 'title')
      ->xpath('title')
      ->callbacks('trim');

    $this
      ->addFieldMapping('field_kinyarwanda_title', 'kinyarwanda_title')
      ->xpath('kinyarwanda_title')
      ->callbacks('trim');

    $this
      ->addFieldMapping('body', 'body')
      ->xpath('body');

    $this
      ->addFieldMapping('field_kinyarwanda_body', 'kinyarwanda_body')
      ->xpath('kinyarwanda_body');

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

}
