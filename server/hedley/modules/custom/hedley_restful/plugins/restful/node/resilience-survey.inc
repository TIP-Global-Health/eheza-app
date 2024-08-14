<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Resilience Survey'),
  'resource' => 'resilience-survey',
  'name' => 'resilience-survey',
  'entity_type' => 'node',
  'bundle' => 'resilience_survey',
  'description' => t('Exports the Resilience Survey bundle.'),
  'class' => 'HedleyRestfulResilienceSurvey',
  'authentication_types' => TRUE,
);
