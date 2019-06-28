<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Breast Exams'),
  'resource' => 'breast-exams',
  'name' => 'breast-exams',
  'entity_type' => 'node',
  'bundle' => 'breast_exam',
  'description' => t('Exports the Breast Exam bundle.'),
  'class' => 'HedleyRestfulBreastExams',
  'authentication_types' => TRUE,
);
