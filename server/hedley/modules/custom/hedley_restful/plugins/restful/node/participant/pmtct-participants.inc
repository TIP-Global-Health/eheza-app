<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('PMTCT Participants'),
  'resource' => 'pmtct-participants',
  'name' => 'pmtct-participants',
  'entity_type' => 'node',
  'bundle' => 'pmtct_participant',
  'description' => t('Exports the PMTCT Participant bundle.'),
  'class' => 'HedleyRestfulPmtctParticipants',
  'authentication_types' => TRUE,
);
