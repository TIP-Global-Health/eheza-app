<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Core Exam'),
  'resource' => 'acute-illness-core-exam',
  'name' => 'acute-illness-core-exam',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_core_exam',
  'description' => t('Exports the Acute Illness Core Exam bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessCoreExam',
  'authentication_types' => TRUE,
);
