<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Obstetrical Exams'),
  'resource' => 'obstetrical-exams',
  'name' => 'obstetrical-exams',
  'entity_type' => 'node',
  'bundle' => 'obstetrical_exam',
  'description' => t('Exports the Obstetrical Exam bundle.'),
  'class' => 'HedleyRestfulObstetricalExams',
  'authentication_types' => TRUE,
);
