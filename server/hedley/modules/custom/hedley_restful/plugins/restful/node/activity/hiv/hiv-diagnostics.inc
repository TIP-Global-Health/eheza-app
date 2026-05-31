<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('HIV Diagnostics'),
  'resource' => 'hiv-diagnostics',
  'name' => 'hiv-diagnostics',
  'entity_type' => 'node',
  'bundle' => 'hiv_diagnostics',
  'description' => t('Exports the HIV Diagnostics bundle.'),
  'class' => 'HedleyRestfulHIVDiagnostics',
  'authentication_types' => TRUE,
);
