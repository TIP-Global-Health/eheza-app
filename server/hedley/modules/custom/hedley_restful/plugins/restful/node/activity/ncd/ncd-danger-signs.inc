<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Danger Signs'),
  'resource' => 'ncd-danger-signs',
  'name' => 'ncd-danger-signs',
  'entity_type' => 'node',
  'bundle' => 'ncd_danger_signs',
  'description' => t('Exports the NCD Danger Signs bundle.'),
  'class' => 'HedleyRestfulNCDDangerSigns',
  'authentication_types' => TRUE,
);
