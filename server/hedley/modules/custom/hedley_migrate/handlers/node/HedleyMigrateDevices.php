<?php

/**
 * @file
 * Contains \HedleyMigrateDevices.
 */

/**
 * Class HedleyMigrateDevices.
 */
class HedleyMigrateDevices extends HedleyMigrateBase {

  /**
   * {@inheritdoc}
   */
  public $entityType = 'node';

  /**
   * {@inheritdoc}
   */
  public $bundle = 'device';

  /**
   * {@inheritdoc}
   */
  protected $csvColumns = [
    'id',
    'title',
    'field_pairing_code',
  ];

  /**
   * {@inheritdoc}
   */
  protected $simpleMappings = [
    'field_pairing_code',
  ];

}
