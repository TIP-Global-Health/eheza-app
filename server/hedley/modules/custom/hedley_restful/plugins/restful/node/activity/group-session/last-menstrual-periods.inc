<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Last Menstrual Periods'),
  'resource' => 'last-menstrual-periods',
  'name' => 'last-menstrual-periods',
  'entity_type' => 'node',
  'bundle' => 'last_menstrual_period',
  'description' => t('Exports the Last Menstrual Period bundle.'),
  'class' => 'HedleyRestfulLastMenstrualPeriods',
  'authentication_types' => TRUE,
);
