<?php

/**
 * @file
 * Restful plugin.
 */

$plugin = array(
  'label' => t('Counseling Topics'),
  'resource' => 'counseling-topics',
  'name' => 'counseling-topics',
  'entity_type' => 'node',
  'bundle' => 'counseling_topic',
  'description' => t('Exports the Counseling Topic bundle.'),
  'class' => 'HedleyRestfulCounselingTopics',
  'authentication_types' => TRUE,
);
