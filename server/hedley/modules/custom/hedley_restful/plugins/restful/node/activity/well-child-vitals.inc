<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Vitals'),
  'resource' => 'well-child-vitals',
  'name' => 'well-child-vitals',
  'entity_type' => 'node',
  'bundle' => 'well_child_vitals',
  'description' => t('Exports the Well Child Vitals bundle.'),
  'class' => 'HedleyRestfulWellChildVitals',
  'authentication_types' => TRUE,
);
