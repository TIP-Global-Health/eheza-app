<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Attendances'),
  'resource' => 'attendances',
  'name' => 'attendances',
  'entity_type' => 'node',
  'bundle' => 'attendance',
  'description' => t('Exports the Attendance bundle.'),
  'class' => 'HedleyRestfulAttendances',
  'authentication_types' => TRUE,
);
