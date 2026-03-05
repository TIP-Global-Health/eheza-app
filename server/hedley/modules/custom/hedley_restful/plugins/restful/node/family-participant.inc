<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Family Participant'),
  'resource' => 'family-participant',
  'name' => 'family-participant',
  'entity_type' => 'node',
  'bundle' => 'family_participant',
  'description' => t('Exports the Family Participant bundle.'),
  'class' => 'HedleyRestfulFamilyParticipant',
  'authentication_types' => TRUE,
);
