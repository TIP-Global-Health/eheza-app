<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Liver Function Test'),
  'resource' => 'ncd-liver-function-test',
  'name' => 'ncd-liver-function-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_liver_function_test',
  'description' => t('Exports the NCD Liver Function Test bundle.'),
  'class' => 'HedleyRestfulNCDLiverFunctionTest',
  'authentication_types' => TRUE,
);
