<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Creatinine Test'),
  'resource' => 'ncd-creatinine-test',
  'name' => 'ncd-creatinine-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_creatinine_test',
  'description' => t('Exports the NCD Creatinine Test bundle.'),
  'class' => 'HedleyRestfulNCDCreatinineTest',
  'authentication_types' => TRUE,
);
