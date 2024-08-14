<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Tuberculosis Diagnostics'),
  'resource' => 'tuberculosis-diagnostics',
  'name' => 'tuberculosis-diagnostics',
  'entity_type' => 'node',
  'bundle' => 'tuberculosis_diagnostics',
  'description' => t('Exports the Tuberculosis Diagnostics bundle.'),
  'class' => 'HedleyRestfulTuberculosisDiagnostics',
  'authentication_types' => TRUE,
);
