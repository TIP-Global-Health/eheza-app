<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Obstetric Histories Step2'),
  'resource' => 'obstetric-histories-step2',
  'name' => 'obstetric-histories-step2',
  'entity_type' => 'node',
  'bundle' => 'obstetric_history_step2',
  'description' => t('Exports the Obstetric History second step bundle.'),
  'class' => 'HedleyRestfulObstetricHistoriesStep2',
  'authentication_types' => TRUE,
);
