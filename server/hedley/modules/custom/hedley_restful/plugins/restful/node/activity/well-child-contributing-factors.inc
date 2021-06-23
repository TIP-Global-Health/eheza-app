<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Contributing Factors'),
  'resource' => 'well-child-contributing-factors',
  'name' => 'well-child-contributing-factors',
  'entity_type' => 'node',
  'bundle' => 'well_child_contributing_factors',
  'description' => t('Exports the WellChildContributingFactors bundle.'),
  'class' => 'HedleyRestfulWellChildContributingFactors',
  'authentication_types' => TRUE,
);
