<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Recommended Treatment'),
  'resource' => 'prenatal-recommended-treatment',
  'name' => 'prenatal-recommended-treatment',
  'entity_type' => 'node',
  'bundle' => 'prenatal_recommended_treatment',
  'description' => t('Exports the Prenatal Nutrition bundle.'),
  'class' => 'HedleyRestfulPrenatalRecommendedTreatment',
  'authentication_types' => TRUE,
);
