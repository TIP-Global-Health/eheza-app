<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Pregnancy Test'),
  'resource' => 'ncd-pregnancy-test',
  'name' => 'ncd-pregnancy-test',
  'entity_type' => 'node',
  'bundle' => 'ncd_pregnancy_test',
  'description' => t('Exports the NCD Pregnancy Test bundle.'),
  'class' => 'HedleyRestfulNCDPregnancyTest',
  'authentication_types' => TRUE,
);
