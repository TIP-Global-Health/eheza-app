<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Group NCDA'),
  'resource' => 'group-ncda',
  'name' => 'group-ncda',
  'entity_type' => 'node',
  'bundle' => 'group_ncda',
  'description' => t('Exports the Group NCDA bundle.'),
  'class' => 'HedleyRestfulGroupNCDA',
  'authentication_types' => TRUE,
);
