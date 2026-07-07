<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Prenatal Breastfeeding'),
  'resource' => 'prenatal-breastfeeding',
  'name' => 'prenatal-breastfeeding',
  'entity_type' => 'node',
  'bundle' => 'prenatal_breastfeeding',
  'description' => t('Exports the Prenatal Breastfeeding bundle.'),
  'class' => 'HedleyRestfulPrenatalBreastfeeding',
  'authentication_types' => TRUE,
);
