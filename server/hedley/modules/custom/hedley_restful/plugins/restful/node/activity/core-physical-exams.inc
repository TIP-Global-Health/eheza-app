<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Core Physical Exams'),
  'resource' => 'core-physical-exams',
  'name' => 'core-physical-exams',
  'entity_type' => 'node',
  'bundle' => 'core_physical_exam',
  'description' => t('Exports the Core Physical Exam bundle.'),
  'class' => 'HedleyRestfulCorePhysicalExams',
  'authentication_types' => TRUE,
);
