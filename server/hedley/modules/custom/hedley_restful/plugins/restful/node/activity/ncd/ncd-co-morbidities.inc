<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('NCD Co-Morbidities'),
  'resource' => 'ncd-co-morbidities',
  'name' => 'ncd-co-morbidities',
  'entity_type' => 'node',
  'bundle' => 'ncd_co_morbidities',
  'description' => t('Exports the NCD Co-Morbidities bundle.'),
  'class' => 'HedleyRestfulNCDCoMorbidities',
  'authentication_types' => TRUE,
);
