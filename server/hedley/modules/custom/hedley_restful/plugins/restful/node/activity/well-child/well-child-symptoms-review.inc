<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Well Child Symptoms Review'),
  'resource' => 'well-child-symptoms-review',
  'name' => 'well-child-symptoms-review',
  'entity_type' => 'node',
  'bundle' => 'well_child_symptoms_review',
  'description' => t('Exports the Well Child Symptoms Review bundle.'),
  'class' => 'HedleyRestfulWellChildSymptomsReview',
  'authentication_types' => TRUE,
);
