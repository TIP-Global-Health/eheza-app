<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child NCDA'),
  'resource' => 'well-child-ncda',
  'name' => 'well-child-ncda',
  'entity_type' => 'node',
  'bundle' => 'well_child_ncda',
  'description' => t('Exports the Well Child NCDA bundle.'),
  'class' => 'HedleyRestfulWellChildNCDA',
  'authentication_types' => TRUE,
);
