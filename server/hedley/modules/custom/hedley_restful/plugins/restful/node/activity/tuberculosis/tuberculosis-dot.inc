<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis DOT'),
  'resource' => 'tuberculosis-dot',
  'name' => 'tuberculosis-dot',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_dot',
  'description' => t('Exports the Tuberculosis DOT bundle.'),
  'class' => 'HedleyRestfulTuberculosisDOT',
  'authentication_types' => TRUE,
);
