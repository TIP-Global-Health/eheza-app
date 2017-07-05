<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Group'),
  'resource' => 'group',
  'name' => 'group',
  'entity_type' => 'node',
  'bundle' => 'group',
  'description' => t('Exports the Group bundle.'),
  'class' => 'HedleyRestfulGroup',
  'authentication_types' => TRUE,
);
