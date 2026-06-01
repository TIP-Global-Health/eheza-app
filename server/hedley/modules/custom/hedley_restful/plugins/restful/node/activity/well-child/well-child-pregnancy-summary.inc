<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child PregnancySummary'),
  'resource' => 'well-child-pregnancy-summary',
  'name' => 'well-child-pregnancy-summary',
  'entity_type' => 'node',
  'bundle' => 'well_child_pregnancy_summary',
  'description' => t('Exports the Well Child Pregnancy Summary bundle.'),
  'class' => 'HedleyRestfulWellChildPregnancySummary',
  'authentication_types' => TRUE,
);
