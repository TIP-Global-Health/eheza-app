<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Contributing Factors'),
  'resource' => 'contributing-factors',
  'name' => 'contributing-factors',
  'entity_type' => 'node',
  'bundle' => 'contributing_factors',
  'description' => t('Exports the ContributingFactors bundle.'),
  'class' => 'HedleyRestfulContributingFactors',
  'authentication_types' => TRUE,
);
