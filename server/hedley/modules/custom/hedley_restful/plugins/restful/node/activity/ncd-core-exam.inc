<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Core Exam'),
  'resource' => 'ncd-core-exam',
  'name' => 'ncd-core-exam',
  'entity_type' => 'node',
  'bundle' => 'ncd_core_exam',
  'description' => t('Exports the NCD Core Exam bundle.'),
  'class' => 'HedleyRestfulNCDCoreExam',
  'authentication_types' => TRUE,
);
