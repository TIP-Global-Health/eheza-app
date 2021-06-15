<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child encounters'),
  'resource' => 'well-child-encounter',
  'name' => 'well-child-encounter',
  'entity_type' => 'node',
  'bundle' => 'well_child_encounter',
  'description' => t('Exports the Well Child Encounter bundle.'),
  'class' => 'HedleyRestfulIndividualEncounter',
  'authentication_types' => TRUE,
);
