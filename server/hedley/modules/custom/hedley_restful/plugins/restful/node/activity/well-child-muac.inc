<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Muac'),
  'resource' => 'well-child-muac',
  'name' => 'well-child-muac',
  'entity_type' => 'node',
  'bundle' => 'well_child_muac',
  'description' => t('Exports the Well Child MUAC bundle.'),
  'class' => 'HedleyRestfulWellChildMuac',
  'authentication_types' => TRUE,
);
