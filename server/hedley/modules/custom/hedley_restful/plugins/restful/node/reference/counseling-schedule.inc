<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Counseling Schedule'),
  'resource' => 'counseling-schedule',
  'name' => 'counseling-schedule',
  'entity_type' => 'node',
  'bundle' => 'counseling_schedule',
  'description' => t('Exports the Counseling Schedule bundle.'),
  'class' => 'HedleyRestfulCounselingSchedule',
  'authentication_types' => TRUE,
);
