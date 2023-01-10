<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD HbA1c Test'),
  'resource' => 'ncd-hba1c-test',
  'name' => 'ncd-hba1c-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_hba1c_test',
  'description' => t('Exports the NCD HbA1c Test bundle.'),
  'class' => 'HedleyRestfulNCDHbA1cTest',
  'authentication_types' => TRUE,
);
