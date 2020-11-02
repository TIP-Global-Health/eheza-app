<?php

/**
 * @file
 * Drush commands collection.
 */

/**
 * Implements hook_drush_command().
 */
function hedley_patient_drush_command() {
  $items = [];
  $items['consolidate-patients'] = [
    'callback' => 'hedley_patient_consolidate_patients',
    'description' => 'Consolidates data of 2 patient - used when there are duplicate instances of same patient.',
    'options' => [
      'original' => 'Patient to which all data is consolidated.',
      'duplicate' => 'Patient which data is moved to the original patient.'
    ],
    'aliases' => ['cns-ptnts'],
  ];

  return $items;
}

function hedley_patient_consolidate_patients() {
  $original = drush_get_option('original');
  $duplicate = drush_get_option('duplicate');

  if (empty($original) || empty($duplicate)) {
    drush_print('You have to specify both --original and --duplicate parameter.');
    exit;
  }

  $wrapper_original = entity_metadata_wrapper('node', $original);
  $wrapper_duplicate = entity_metadata_wrapper('node', $duplicate);

  if ($wrapper_original->getBundle() !== 'person') {
    drush_print('Error! Original patient node type is not person. Please fix and try again.');
    exit;
  }  

  if ($wrapper_duplicate->getBundle() !== 'person') {
    drush_print('Error! Duplicate patient node type is not person. Please fix and try again.');
    exit;
  }

  drush_print($wrapper_duplicate->getBundle());
}

