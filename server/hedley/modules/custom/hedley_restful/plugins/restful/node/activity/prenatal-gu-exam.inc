<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal GU Exam'),
  'resource' => 'prenatal-gu-exam',
  'name' => 'prenatal-gu-exam',
  'entity_type' => 'node',
  'bundle' => 'prenatal_gu_exam',
  'description' => t('Exports the Prenatal GU Exam bundle.'),
  'class' => 'HedleyRestfulPrenatalGUExam',
  'authentication_types' => TRUE,
);
