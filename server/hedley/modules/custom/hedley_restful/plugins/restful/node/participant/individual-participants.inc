<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Individual Participants'),
  'resource' => 'individual-participants',
  'name' => 'individual-participants',
  'entity_type' => 'node',
  'bundle' => 'individual_participant',
  'description' => t('Exports the Individual Participant bundle.'),
  'class' => 'HedleyRestfulIndividualParticipants',
  'authentication_types' => TRUE,
);
