<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD HIV Test'),
  'resource' => 'ncd-hiv-test',
  'name' => 'ncd-hiv-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_hiv_test',
  'description' => t('Exports the NCD HIV Test bundle.'),
  'class' => 'HedleyRestfulNCDHIVTest',
  'authentication_types' => TRUE,
);
