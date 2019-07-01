<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Danger Signs'),
  'resource' => 'danger-signs',
  'name' => 'danger-signs',
  'entity_type' => 'node',
  'bundle' => 'danger_signs',
  'description' => t('Exports the Danger Signs bundle.'),
  'class' => 'HedleyRestfulDangerSigns',
  'authentication_types' => TRUE,
);
