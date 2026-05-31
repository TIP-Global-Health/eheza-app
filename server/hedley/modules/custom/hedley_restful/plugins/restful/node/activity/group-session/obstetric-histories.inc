<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Obstetric Histories'),
  'resource' => 'obstetric-histories',
  'name' => 'obstetric-histories',
  'entity_type' => 'node',
  'bundle' => 'obstetric_history',
  'description' => t('Exports the Obstetric History first step bundle.'),
  'class' => 'HedleyRestfulObstetricHistories',
  'authentication_types' => TRUE,
);
