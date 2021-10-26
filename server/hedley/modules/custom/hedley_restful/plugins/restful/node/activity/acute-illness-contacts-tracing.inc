<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Acute Illness Contacts Tracing'),
  'resource' => 'acute-illness-contacts-tracing',
  'name' => 'acute-illness-contacts-tracing',
  'entity_type' => 'node',
  'bundle' => 'acute_illness_contacts_tracing',
  'description' => t('Exports the AcuteIllnessContactsTracing bundle.'),
  'class' => 'HedleyRestfulAcuteIllnessContactsTracing',
  'authentication_types' => TRUE,
);
